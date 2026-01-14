# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from dataclasses import dataclass
import logging
import typing
from typing import Optional

from jinja2 import Environment
import keyword_generation.data_model as data_model
from keyword_generation.data_model.keyword_data import Card, KeywordData
from keyword_generation.generators.template_context import KeywordTemplateContext
from keyword_generation.handlers.registry import HandlerRegistry, create_default_registry
from keyword_generation.utils import (
    get_license_header,
    handle_single_word_keyword,
)
from keyword_generation.utils.domain_mapper import get_keyword_domain
from keyword_generation.utils.keyword_utils import KeywordNames
from output_manager import OutputManager

logger = logging.getLogger(__name__)


def _get_source_keyword(keyword: str, settings: typing.Dict[str, typing.Any]) -> str:
    """Get the 'source' keyword to look up in LSPP structs.  Usually
     its the keyword that its passed in, but in cases where one LSPP
    keyword is generated into multiple classes - such as for
    LOAD_SEGMENT => (LOAD_SEGMENT, LOAD_SEGMENT_ID) - this could be
    overwritten by the "source-keyword" property.
    """
    source_keyword: str = settings.get("source-keyword", keyword)
    if source_keyword != keyword:
        logger.debug(f"Using source keyword '{source_keyword}' for '{keyword}'")
    return source_keyword


def _create_template_context(
    classname: str, keyword: str, keyword_options: typing.Dict, license: str
) -> KeywordTemplateContext:
    """
    Create a structured template context object for keyword rendering.

    This replaces the old _get_jinja_variable dict building with a typed context object.

    Args:
        classname: Python class name for the keyword
        keyword: Full keyword string (e.g., "SECTION_SHELL_TITLE")
        keyword_options: Keyword configuration from manifest.json
        license: License header text

    Returns:
        KeywordTemplateContext instance ready for template rendering
    """
    source_keyword = _get_source_keyword(keyword, keyword_options)
    generation_settings = keyword_options.get("generation-options", {})
    initial_labels = keyword_options.get("labels", None)
    keyword_data = _get_keyword_data(keyword, source_keyword, generation_settings, initial_labels=initial_labels)
    keyword_data.classname = classname

    # Determine alias if present
    alias = data_model.get_alias(keyword)
    alias_subkeyword = None
    if alias:
        alias_tokens = alias.split("_")
        alias = KeywordNames.from_keyword(alias).classname
        alias_subkeyword = "_".join(alias_tokens[1:])

    return KeywordTemplateContext(
        license=license,
        keyword_data=keyword_data,
        alias=alias,
        alias_subkeyword=alias_subkeyword,
    )


def _transform_data(data: KeywordData):
    """Applies transformations to normalize keyword data for Python code generation.

    Uses polymorphic iteration via KeywordData.get_all_cards() to handle:
    - Regular cards
    - Table card groups (repeating card structures)
    - Card sets (source cards and their options)
    - Top-level option cards

    Each field is normalized via Field.normalize() which handles:
    - Lowercase field names (SECID -> secid)
    - Python type mapping (integer->int, real->float)
    - Reserved keyword handling (as -> as_)
    - Default value conversion
    - Help text cleanup
    """
    # Get all cards from all structures (main cards, card_sets, options)
    all_cards = data.get_all_cards()

    # Normalize all fields in all cards
    for card in all_cards:
        # card may be Card instance or dict during transition
        if isinstance(card, Card):
            fields = card.get_all_fields()
        else:
            # Fallback for dict-based cards - handle table_group case
            if card.get("table_group") and card.get("sub_cards"):
                # For table_group dicts, aggregate fields from all sub_cards
                fields = []
                for sub_card in card["sub_cards"]:
                    fields.extend(sub_card.get("fields", []))
            else:
                fields = card.get("fields", [])

        for field in fields:
            field.normalize()

    # Assign indices to main cards
    for index, card in enumerate(data.cards):
        card["index"] = index


def _get_insertion_index_for_cards(
    requested_index: int, container: typing.Union[typing.List[Card], typing.List[typing.Dict]]
) -> int:
    """Find insertion index in container, handling both Card instances and dicts during transition."""
    for index, card in enumerate(container):
        card_index = card.get("source_index", card["index"])
        if card_index == requested_index:
            # we are inserting right before this card, store the index
            return index
    # insertion index not found, it must be past the end, in which case
    # the insertion index is treated literally
    return requested_index


def _do_insertions(kwd_data: KeywordData) -> None:
    # [(a,b,c)] => insert b into c at index a
    insertion_targets: typing.List[typing.Tuple[int, typing.Dict, typing.List]] = []
    logger.debug(f"Processing {len(kwd_data.card_insertions)} card insertions")
    for insertion in kwd_data.card_insertions:
        insertion: data_model.Insertion = insertion
        insertion_index = insertion.target_index
        insertion_name = insertion.target_class
        insertion_card = insertion.card

        # Validate that insertion has required fields
        if insertion_index is None or insertion_card is None or insertion_name is None:
            logger.warning(
                f"Skipping insertion with missing fields: index={insertion_index}, "
                f"card={insertion_card}, name={insertion_name}"
            )
            continue

        if insertion_name == "":
            # insert directly into keyword data
            container = kwd_data.cards
            index = _get_insertion_index_for_cards(insertion_index, container)
            logger.debug(f"Inserting card at index {index} into keyword cards")
            insertion_targets.append((index, insertion_card, container))
        else:
            # insert into another card set - may be CardSetsContainer or dict
            if kwd_data.card_sets:
                sets = (
                    kwd_data.card_sets.sets if hasattr(kwd_data.card_sets, "sets") else kwd_data.card_sets["sets"]
                )  # type: ignore
                card_sets = [
                    cs for cs in sets if (cs.name if hasattr(cs, "name") else cs["name"]) == insertion_name
                ]  # type: ignore
                logger.debug(f"Inserting card into {len(card_sets)} card sets matching '{insertion_name}'")
                for card_set in card_sets:
                    container = (
                        card_set.source_cards if hasattr(card_set, "source_cards") else card_set["source_cards"]
                    )  # type: ignore
                    index = _get_insertion_index_for_cards(insertion_index, container)
                    insertion_targets.append((index, insertion_card, container))
    for index, item, container in insertion_targets:
        container.insert(index, item)


def _delete_marked_indices(kwd_data: KeywordData) -> None:
    marked_indices = []
    for index, card in enumerate(kwd_data.cards):
        # Check if mark_for_removal is set to a truthy value (not None, not 0)
        if card.get("mark_for_removal"):
            marked_indices.append(index)
    # removal will affect order if we iterate forwards, so iterate backwards
    marked_indices.sort(reverse=True)
    if marked_indices:
        logger.debug(f"Deleting {len(marked_indices)} marked cards at indices: {marked_indices}")
    for index in marked_indices:
        del kwd_data.cards[index]

    options_list = kwd_data.options or []
    if len(options_list) > 0:
        marked_option_indices = []
        for index, option in enumerate(options_list):
            # option may be OptionGroup instance or dict - check for truthy mark_for_removal value
            has_mark = hasattr(option, "get") and option.get("mark_for_removal")  # type: ignore
            if has_mark:
                marked_option_indices.append(index)
        marked_option_indices.sort(reverse=True)
        if marked_option_indices:
            logger.debug(f"Deleting {len(marked_option_indices)} marked options at indices: {marked_option_indices}")
        for index in marked_option_indices:
            del options_list[index]
        if len(options_list) == 0:
            kwd_data.options = []  # Set to empty list instead of None


def _add_indices(kwd_data: KeywordData) -> None:
    # handlers might point to cards by a specific index.
    for index, card in enumerate(kwd_data.cards):
        card["index"] = index


def _prepare_for_insertion(kwd_data: KeywordData) -> None:
    kwd_data.card_insertions = []


def _add_option_indices(kwd_data: KeywordData) -> None:
    index = len(kwd_data.cards)
    for options in kwd_data.options or []:
        # options may be OptionGroup instance or dict
        cards = options.cards if hasattr(options, "cards") else options["cards"]  # type: ignore
        for card in cards:
            card["index"] = index
        index += 1


def _after_handle(kwd_data: KeywordData, registry: HandlerRegistry) -> None:
    # TODO - move these to their respective handler
    _do_insertions(kwd_data)
    _delete_marked_indices(kwd_data)
    _add_option_indices(kwd_data)
    registry.post_process_all(kwd_data)  # Pass KeywordData instance for post_process


def _before_handle(kwd_data: KeywordData) -> None:
    _add_indices(kwd_data)
    _prepare_for_insertion(kwd_data)


def _handle_keyword_data(
    kwd_data: KeywordData,
    settings: typing.Dict[str, typing.Any],
    initial_labels: typing.Optional[typing.Dict[str, int]] = None,
) -> None:
    """Process keyword data through handler pipeline.

    All pipeline stages now work directly with KeywordData instances using
    attribute access. No more dict conversions needed!

    Args:
        kwd_data: The keyword data to process
        settings: Handler settings from manifest's "generation-options"
        initial_labels: Optional label mappings from manifest's "labels" section
    """
    registry = create_default_registry()

    logger.debug(f"Handling keyword data with {len(kwd_data.cards)} cards")
    _before_handle(kwd_data)

    # Run handlers with KeywordData instance, passing initial labels
    registry.apply_all(kwd_data, settings, initial_labels=initial_labels)

    # After-handle processing
    _after_handle(kwd_data, registry)

    logger.debug(f"Keyword data handling complete, final card count: {len(kwd_data.cards)}")


@dataclass
class LinkFieldInfo:
    """Information about a link field."""

    name: str
    is_table_group: bool = False
    is_table: bool = False  # True for TableCard (not TableCardGroup)
    table_name: Optional[str] = None
    key_field: Optional[str] = None


def _convert_link_fields_to_template_format(
    link_fields: typing.List[LinkFieldInfo],
) -> typing.List[typing.Dict[str, typing.Any]]:
    """Convert LinkFieldInfo objects to template-friendly dict format."""
    return [
        {
            "name": f.name,
            "is_table_group": f.is_table_group,
            "is_table": f.is_table,
            "table_name": f.table_name,
            "key_field": f.key_field,
        }
        for f in link_fields
    ]


def _add_define_transform_link_data(link_data: typing.List[typing.Dict], link_fields: typing.List[LinkFieldInfo]):
    transform_link_data = {
        "classname": "DefineTransformation",
        "modulename": "define.define_transformation",
        "keyword_type": "DEFINE",
        "keyword_subtype": "TRANSFORMATION",
        "fields": _convert_link_fields_to_template_format(link_fields),
        "linkid": "tranid",
        "link_type_name": "DEFINE_TRANSFORMATION",
    }
    link_data.append(transform_link_data)


def _add_define_curve_link_data(link_data: typing.List[typing.Dict], link_fields: typing.List[LinkFieldInfo]):
    curve_link_data = {
        "classname": "DefineCurve",
        "modulename": "define.define_curve",
        "keyword_type": "DEFINE",
        "keyword_subtype": "CURVE",
        "fields": _convert_link_fields_to_template_format(link_fields),
        "linkid": "lcid",
        "link_type_name": "DEFINE_CURVE",
    }
    link_data.append(curve_link_data)


class LinkIdentity:
    """Identifies the type of link a field references.

    These values correspond to the "link" field values in kwd.json.
    """

    MAT = 14
    SECTION = 15
    DEFINE_CURVE = 19
    DEFINE_TRANSFORMATION = 40
    PART = 69
    DEFINE_CURVE_OR_TABLE = 86


def _add_mat_link_data(link_data: typing.List[typing.Dict], link_fields: typing.List[LinkFieldInfo]):
    """Add link data for MAT_* material keywords."""
    mat_link_data = {
        "classname": "KeywordBase",
        "modulename": None,
        "keyword_type": "MAT",
        "keyword_subtype": None,
        "fields": _convert_link_fields_to_template_format(link_fields),
        "linkid": "mid",
        "link_type_name": "MAT",
        "is_polymorphic": True,
    }
    link_data.append(mat_link_data)


def _add_section_link_data(link_data: typing.List[typing.Dict], link_fields: typing.List[LinkFieldInfo]):
    """Add link data for SECTION_* keywords."""
    section_link_data = {
        "classname": "KeywordBase",
        "modulename": None,
        "keyword_type": "SECTION",
        "keyword_subtype": None,
        "fields": _convert_link_fields_to_template_format(link_fields),
        "linkid": "secid",
        "link_type_name": "SECTION",
        "is_polymorphic": True,
    }
    link_data.append(section_link_data)


def _add_part_link_data(link_data: typing.List[typing.Dict], link_fields: typing.List[LinkFieldInfo]):
    """Add link data for PART keywords (table lookup pattern).

    PART links are special because PART keywords contain multiple parts in a
    DataFrame. The lookup must search `kwd.parts["pid"]` to find which Part
    keyword contains the referenced pid.
    """
    part_link_data = {
        "classname": "KeywordBase",
        "modulename": None,
        "keyword_type": "PART",
        "keyword_subtype": None,
        "fields": _convert_link_fields_to_template_format(link_fields),
        "linkid": "pid",
        "link_type_name": "PART",
        "is_polymorphic": True,
        "is_table_lookup": True,  # Triggers special lookup in parts DataFrame
    }
    link_data.append(part_link_data)


def _add_define_curve_or_table_link_data(link_data: typing.List[typing.Dict], link_fields: typing.List[LinkFieldInfo]):
    """Add link data for polymorphic DEFINE_CURVE or DEFINE_TABLE fields."""
    polymorphic_link_data = {
        "classname": "KeywordBase",
        "modulename": None,
        "keyword_type": None,
        "keyword_subtype": None,
        "fields": _convert_link_fields_to_template_format(link_fields),
        "linkid": None,
        "link_type_name": "DEFINE_CURVE_OR_TABLE",
        "is_polymorphic": True,
        "is_multi_target": True,
        "targets": [
            {"type": "DEFINE", "subtype": "CURVE", "id_field": "lcid"},
            {"type": "DEFINE", "subtype": "TABLE", "id_field": "tbid"},
        ],
    }
    link_data.append(polymorphic_link_data)


def _get_links(kwd_data: KeywordData) -> typing.Optional[typing.Dict]:
    links = {
        LinkIdentity.MAT: [],
        LinkIdentity.SECTION: [],
        LinkIdentity.DEFINE_CURVE: [],
        LinkIdentity.DEFINE_TRANSFORMATION: [],
        LinkIdentity.PART: [],
        LinkIdentity.DEFINE_CURVE_OR_TABLE: [],
    }
    has_link = False
    for card in kwd_data.cards:
        # Check if this is a table_group card (TableCardGroup)
        is_table_group = card.table_group if isinstance(card, Card) else card.get("table_group", False)
        table_name = card.overall_name if isinstance(card, Card) else card.get("overall_name")
        key_field = card.key_field if isinstance(card, Card) else card.get("key_field")

        # Check if this is a table card (TableCard - simple repeating card)
        table_meta = card.table if isinstance(card, Card) else card.get("table")
        is_table = table_meta is not None
        if is_table and not table_name:
            # For TableCard, table name comes from the metadata
            table_name = table_meta.name if hasattr(table_meta, "name") else table_meta.get("name")

        # Use card.get_all_fields() instead of _get_fields helper
        fields = card.get_all_fields() if isinstance(card, Card) else card.get("fields", [])
        for field in fields:
            if "link" not in field:
                continue
            link = field["link"]
            if link not in links.keys():
                continue
            has_link = True
            # Use property_name for Python-safe identifiers (handles special chars like /)
            prop_name = field.get("property_name") or field["name"]
            field_info = LinkFieldInfo(
                name=prop_name,
                is_table_group=is_table_group,
                is_table=is_table,
                table_name=table_name,
                key_field=key_field,
            )
            links[link].append(field_info)
    if not has_link:
        return None
    return links


def _add_links(kwd_data: KeywordData) -> None:
    """Add "links", or properties that link one keyword to another."""
    links = _get_links(kwd_data)
    if links is None:
        logger.debug("No links found for this keyword")
        return
    link_data = []
    link_count = 0
    for link_type, link_fields in links.items():
        if not link_fields:
            continue
        if link_type == LinkIdentity.MAT:
            _add_mat_link_data(link_data, link_fields)
            link_count += len(link_fields)
        elif link_type == LinkIdentity.SECTION:
            _add_section_link_data(link_data, link_fields)
            link_count += len(link_fields)
        elif link_type == LinkIdentity.DEFINE_TRANSFORMATION:
            _add_define_transform_link_data(link_data, link_fields)
            link_count += len(link_fields)
        elif link_type == LinkIdentity.DEFINE_CURVE:
            _add_define_curve_link_data(link_data, link_fields)
            link_count += len(link_fields)
        elif link_type == LinkIdentity.PART:
            _add_part_link_data(link_data, link_fields)
            link_count += len(link_fields)
        elif link_type == LinkIdentity.DEFINE_CURVE_OR_TABLE:
            _add_define_curve_or_table_link_data(link_data, link_fields)
            link_count += len(link_fields)
    kwd_data.links = link_data
    logger.debug(f"Added {link_count} links to keyword data")


def _get_keyword_data(
    keyword_name: str,
    keyword: str,
    settings: typing.Dict[str, typing.Any],
    initial_labels: typing.Optional[typing.Dict[str, int]] = None,
) -> KeywordData:
    """Gets the keyword data from kwdm. Transforms it based on generation settings
    and default transformations needed to produce valid python code.

    Returns KeywordData dataclass instance - no more dict conversions!

    Args:
        keyword_name: The keyword name (e.g., "MAT_ELASTIC")
        keyword: The source keyword to load from kwd.json
        settings: Handler settings from manifest's "generation-options"
        initial_labels: Optional label mappings from manifest's "labels" section
    """
    logger.debug(f"Getting keyword data for '{keyword_name}' (source: '{keyword}')")
    config = data_model.get_config()
    kwd_data_dict = {"cards": config.keyword_data.get_keyword_data_dict(keyword)}

    # Set keyword identity in dict before converting to KeywordData
    tokens = keyword_name.split("_")
    kwd_data_dict["keyword"] = tokens[0]
    kwd_data_dict["subkeyword"] = "_".join(tokens[1:])
    kwd_data_dict["title"] = handle_single_word_keyword(keyword_name)

    # Convert to KeywordData
    kwd_data = KeywordData.from_dict(kwd_data_dict)

    # transformations based on generation settings - handlers work with KeywordData
    _handle_keyword_data(kwd_data, settings, initial_labels=initial_labels)

    # default transformations to a valid format we need for jinja
    _transform_data(kwd_data)
    _add_links(kwd_data)

    logger.debug(f"Keyword data prepared for '{keyword_name}' with {len(kwd_data.cards)} cards")

    return kwd_data


def generate_class(env: Environment, output_manager: OutputManager, item: typing.Dict):
    """
    Generate a Python keyword class from manifest configuration.

    Args:
        env: Jinja2 environment with loaded templates
        output_manager: Handles writing output files to appropriate directories
        item: Keyword configuration dict with 'name' and 'options' keys

    Returns:
        Tuple of (classname, filename_stem) for the generated class
    """
    keyword = item["name"]
    names = KeywordNames.from_keyword(keyword)
    classname = item["options"].get("classname", names.classname)
    logger.debug(f"Starting generation for class '{classname}' from keyword '{keyword}'")
    try:
        # Create structured template context
        license = get_license_header()
        context = _create_template_context(classname, keyword, item["options"], license)

        # Determine domain and create domain subdirectory
        domain = get_keyword_domain(keyword)
        filename = names.filename + ".py"
        logger.debug(f"Rendering template for {classname} in domain '{domain}'")
        content = env.get_template("keyword.j2").render(**context.to_dict())
        output_manager.write_auto_file(domain, filename, content)
        logger.debug(f"Successfully generated class '{classname}'")
        return classname, names.filename
    except Exception as e:
        logger.error(f"Failure in generating {classname}", exc_info=True)
        raise e
