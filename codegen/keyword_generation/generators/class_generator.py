# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

import collections
import os
import typing

from jinja2 import Environment
import keyword_generation.data_model as data_model
from keyword_generation.handlers.add_option import AddOptionHandler
from keyword_generation.handlers.card_set import CardSetHandler
from keyword_generation.handlers.conditional_card import ConditionalCardHandler
from keyword_generation.handlers.external_card import ExternalCardHandler
from keyword_generation.handlers.handler_base import KeywordHandler
from keyword_generation.handlers.insert_card import InsertCardHandler
from keyword_generation.handlers.override_field import OverrideFieldHandler
from keyword_generation.handlers.override_subkeyword import OverrideSubkeywordHandler
from keyword_generation.handlers.rename_property import RenamePropertyHandler
from keyword_generation.handlers.reorder_card import ReorderCardHandler
from keyword_generation.handlers.replace_card import ReplaceCardHandler
from keyword_generation.handlers.series_card import SeriesCardHandler
from keyword_generation.handlers.shared_field import SharedFieldHandler
from keyword_generation.handlers.skip_card import SkipCardHandler
from keyword_generation.handlers.table_card import TableCardHandler
from keyword_generation.handlers.table_card_group import TableCardGroupHandler
from keyword_generation.utils import fix_keyword, get_classname, get_license_header, handle_single_word_keyword
from keyword_generation.utils.domain_mapper import get_keyword_domain


def _get_source_keyword(keyword, settings):
    """Get the 'source' keyword to look up in LSPP structs.  Usually
     its the keyword that its passed in, but in cases where one LSPP
    keyword is generated into multiple classes - such as for
    LOAD_SEGMENT => (LOAD_SEGMENT, LOAD_SEGMENT_ID) - this could be
    overwritten by the "source-keyword" property.
    """
    source_keyword = settings.get("source-keyword", keyword)
    return source_keyword


def _get_jinja_variable(base_variable: typing.Dict) -> typing.Dict:
    jinja_variable = base_variable.copy()
    jinja_variable.update(
        {
            "license": get_license_header(),
            "openbrace": "{",
            "closebrace": "}",
            "repeated_element_types": {"int": "pd.Int32Dtype()", "float": "np.float64", "str": "str"},
        }
    )
    return jinja_variable


def _get_fields(card: typing.Dict) -> typing.List[typing.Dict[str, typing.Any]]:
    if "duplicate_group" in card:
        fields = []
        for sub_card in card["sub_cards"]:
            fields.extend(sub_card["fields"])
        return fields
    return card["fields"]


def _transform_data(data: typing.Dict[str, typing.Any]):
    """applies the following transformations to data:
    - lowercase field names (SECID -> secid)
    - python type mapping (integer->int, real->float)
    """
    type_mapping = {"integer": "int", "real": "float", "string": "str", "real-integer": "float"}

    def fix_fieldname(field_name: str) -> str:
        """returns a python friendly version of field name.
        For example, nx/ida becomes nx_ida, as becomes as_"""
        # deal with bad characters
        for bad_char in ["/", "-", " ", "(", ")", ",", ".", "'", "*", "|", "+"]:
            field_name = field_name.replace(bad_char, "_")
        # deal with reserved statements
        if field_name.lower() in ["global", "as", "int", "lambda", "for"]:
            field_name = field_name + "_"
        if field_name[0].isdigit():
            field_name = "_" + field_name
        return field_name

    def fix_fieldhelp(field_help: str) -> str:
        """help is formatted inside a triple quote,"""
        if field_help.endswith('"'):
            field_help = field_help + " "
        # remove any leading whitespace from each line in field_help
        field_help = "\n".join([l.strip() for l in field_help.split("\n")])
        return field_help

    def fix_field_string_default(field_default: str) -> str:
        """string defaults need to be wrapped in quotes"""
        if field_default == None:
            return None
        return f'"{field_default}"'

    def fix_field_int_default(field_default) -> int:
        """int defaults need to be converted from strings that might be floats"""
        if field_default == None:
            return None
        return int(float(field_default))

    def fix_card(card: typing.Dict) -> None:
        for field in _get_fields(card):
            if "used" not in field:
                field["used"] = True
            field["type"] = type_mapping[field["type"]]
            if not field["used"]:
                field["default"] = None
                field["help"] = ""
                field["name"] = "unused"
                continue
            if field.get("flag", False):
                field["type"] = "bool"
            field_name: str = field["name"]
            fixed_field_name = fix_fieldname(field_name).lower()
            if not "property_name" in field:
                field["property_name"] = fixed_field_name
            field["name"] = field_name.lower()
            if not "property_type" in field:
                field["property_type"] = field["type"]
            if field["type"] == "str":
                if "options" in field:
                    field["options"] = [f'"{option}"' for option in field["options"]]
                field["default"] = fix_field_string_default(field["default"])
            elif field["type"] == "int":
                field["default"] = fix_field_int_default(field["default"])
            field["help"] = fix_fieldhelp(field["help"])

    index = 0
    for card in data["cards"]:
        fix_card(card)
        card["index"] = index
        index = index + 1

    card_sets = data.get("card_sets", {})
    for card_set in card_sets.get("sets", []):
        for card in card_set.get("source_cards", []):
            fix_card(card)
        for option in card_set.get("options", []):
            [fix_card(card) for card in option["cards"]]

    for option in data.get("options", []):
        [fix_card(card) for card in option["cards"]]


def _set_keyword_identity(kwd_data: typing.Dict, keyword_name: str, settings: typing.Dict) -> None:
    tokens = keyword_name.split("_")
    kwd_data["keyword"] = tokens[0]
    kwd_data["subkeyword"] = "_".join(tokens[1:])
    kwd_data["title"] = handle_single_word_keyword(keyword_name)


# functions which return a copy of keyword data after applying the handling specified by the configuration
HANDLERS = collections.OrderedDict(
    {
        "reorder-card": ReorderCardHandler(),
        "table-card": TableCardHandler(),
        "override-field": OverrideFieldHandler(),
        "replace-card": ReplaceCardHandler(),
        "insert-card": InsertCardHandler(),
        "series-card": SeriesCardHandler(),
        "add-option": AddOptionHandler(),
        "card-set": CardSetHandler(),
        "conditional-card": ConditionalCardHandler(),
        "rename-property": RenamePropertyHandler(),
        "skip-card": SkipCardHandler(),
        "table-card-group": TableCardGroupHandler(),
        "external-card-implementation": ExternalCardHandler(),
        "shared-field": SharedFieldHandler(),
        "override-subkeyword": OverrideSubkeywordHandler(),
    }
)


def _get_insertion_index_for_cards(requested_index: int, container):
    for index, card in enumerate(container):
        card_index = card.get("source_index", card["index"])
        if card_index == requested_index:
            # we are inserting right before this card, store the index
            return index
    # insertion index not found, it must be past the end, in which case
    # the insertion index is treated literally
    return requested_index


def _do_insertions(kwd_data):
    # [(a,b,c)] => insert b into c at index a
    insertion_targets: typing.List[typing.Tuple[int, typing.Dict, typing.List]] = []
    for insertion in kwd_data["card_insertions"]:
        insertion: data_model.Insertion = insertion
        insertion_index = insertion.target_index
        insertion_name = insertion.target_class
        insertion_card = insertion.card
        if insertion_name == "":
            # insert directly into keyword data
            container = kwd_data["cards"]
            index = _get_insertion_index_for_cards(insertion_index, container)
            insertion_targets.append((index, insertion_card, container))
        else:
            # insert into another card set
            card_sets = [card_set for card_set in kwd_data["card_sets"]["sets"] if card_set["name"] == insertion_name]
            for card_set in card_sets:
                container = card_set["source_cards"]
                index = _get_insertion_index_for_cards(insertion_index, container)
                insertion_targets.append((index, insertion_card, container))
    for index, item, container in insertion_targets:
        container.insert(index, item)


def _delete_marked_indices(kwd_data):
    marked_indices = []
    for index, card in enumerate(kwd_data["cards"]):
        if "mark_for_removal" in card:
            marked_indices.append(index)
    # removal will affect order if we iterate forwards, so iterate backwards
    marked_indices.sort(reverse=True)
    for index in marked_indices:
        del kwd_data["cards"][index]

    options_list = kwd_data.get("options", [])
    if len(options_list) > 0:
        marked_option_indices = []
        for index, option in enumerate(options_list):
            if "mark_for_removal" in option:
                marked_option_indices.append(index)
        marked_option_indices.sort(reverse=True)
        for index in marked_option_indices:
            del options_list[index]
        if len(options_list) == 0:
            del kwd_data["options"]


def _add_indices(kwd_data):
    # handlers might point to cards by a specific index.
    for index, card in enumerate(kwd_data["cards"]):
        card["index"] = index


def _prepare_for_insertion(kwd_data):
    kwd_data["card_insertions"] = []


def _add_option_indices(kwd_data):
    index = len(kwd_data["cards"])
    for options in kwd_data.get("options", []):
        for card in options["cards"]:
            card["index"] = index
        index += 1


def _after_handle(kwd_data):
    # TODO - move these to their respective handler
    _do_insertions(kwd_data)
    _delete_marked_indices(kwd_data)
    _add_option_indices(kwd_data)
    for handler_name, handler in HANDLERS.items():
        if isinstance(handler, KeywordHandler):
            handler.post_process(kwd_data)


def _before_handle(kwd_data):
    _add_indices(kwd_data)
    _prepare_for_insertion(kwd_data)


def _handle_keyword_data(kwd_data, settings):
    _before_handle(kwd_data)
    # we have to iterate in the order of the handlers because right now the order still matters
    # right now this is only true for reorder_card
    for handler_name, handler in HANDLERS.items():
        handler_settings = settings.get(handler_name)
        if handler_settings == None:
            continue
        handler.handle(kwd_data, handler_settings)
    _after_handle(kwd_data)


def _add_define_transform_link_data(link_data: typing.List[typing.Dict], link_fields: typing.List[str]):
    transform_link_data = {
        "classname": "DefineTransformation",
        "modulename": "definitions.define_transformation",
        "keyword_type": "DEFINE",
        "keyword_subtype": "TRANSFORMATION",
        "fields": link_fields,
        "linkid": "tranid",
    }
    link_data.append(transform_link_data)


class LinkIdentity:
    DEFINE_TRANSFORMATION = 40


def _get_links(kwd_data) -> typing.Optional[typing.Dict]:
    links = {LinkIdentity.DEFINE_TRANSFORMATION: []}
    has_link = False
    for card in kwd_data["cards"]:
        for field in _get_fields(card):
            if "link" not in field:
                continue
            link = field["link"]
            if link not in links.keys():
                continue
            has_link = True
            links[link].append(field["name"])
    if not has_link:
        return None
    return links


def _add_links(kwd_data):
    """Add "links", or properties that link one keyword to another."""
    links = _get_links(kwd_data)
    if links is None:
        return
    link_data = []
    for link_type, link_fields in links.items():
        if link_type == LinkIdentity.DEFINE_TRANSFORMATION:
            _add_define_transform_link_data(link_data, link_fields)
    kwd_data["links"] = link_data


def _get_keyword_data(keyword_name, keyword, settings):
    """Gets the keyword data dict from kwdm.  Transforms it
    based on the generation settings that are passed in, if any,
    and with default transformations that are needed to produce
    valid python code.
    """
    kwd_data = {"cards": data_model.KWDM_INSTANCE.get_keyword_data_dict(keyword)}

    _set_keyword_identity(kwd_data, keyword_name, settings)

    # transformations based on generation settings
    _handle_keyword_data(kwd_data, settings)

    # default transformations to a valid format we need for jinja
    _transform_data(kwd_data)

    _add_links(kwd_data)
    return kwd_data


def _get_base_variable(classname: str, keyword: str, keyword_options: typing.Dict) -> typing.Dict:
    source_keyword = _get_source_keyword(keyword, keyword_options)
    generation_settings = keyword_options.get("generation-options", {})
    keyword_data = _get_keyword_data(keyword, source_keyword, generation_settings)
    keyword_data["classname"] = classname
    alias = data_model.get_alias(keyword)
    alias_subkeyword = None
    if alias:
        alias_tokens = alias.split("_")
        alias = get_classname(fix_keyword(alias))
        alias_subkeyword = "_".join(alias_tokens[1:])
    data = {
        "keyword_data": keyword_data,
        "alias": alias,
        "alias_subkeyword": alias_subkeyword,
    }
    return data


def generate_class(env: Environment, lib_path: str, item: typing.Dict) -> typing.Tuple[str, str]:
    keyword = item["name"]
    fixed_keyword = fix_keyword(keyword)
    classname = item["options"].get("classname", get_classname(fixed_keyword))
    try:
        base_variable = _get_base_variable(classname, keyword, item["options"])
        jinja_variable = _get_jinja_variable(base_variable)

        # Determine domain and create domain subdirectory
        domain = get_keyword_domain(keyword)
        domain_path = os.path.join(lib_path, "auto", domain)
        os.makedirs(domain_path, exist_ok=True)

        filename = os.path.join(domain_path, fixed_keyword.lower() + ".py")
        with open(filename, "w", encoding="utf-8") as f:
            f.write(env.get_template("keyword.j2").render(**jinja_variable))
        return classname, fixed_keyword.lower()
    except Exception as e:
        print(f"Failure in generating {classname}")
        raise e
