# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

"""This script generates the keyword classes for the LSPP.  It uses the kwd.json file to get the keyword"""
import argparse
import collections
import copy
from dataclasses import dataclass
import json
import logging
import os
import pathlib
import shutil
import typing

from jinja2 import Environment, FileSystemLoader

SKIPPED_KEYWORDS = set(
    [
        # defined manually because of the variable length text card
        "DEFINE_FUNCTION",
        # element_solid (10 nodes format) - merging the element solids
        "ELEMENT_SOLID (ten nodes format)",
        "ELEMENT_SOLID",
        "ELEMENT_SOLID_ORTHO (ten nodes format)",
        "ELEMENT_SOLID_ORTHO"
        # issue #184 - this is not documented in the manual
        #"CONTROL_TIMESTEP",CONTROL_TIMESTEP is in the kwd.json now and should be generated issue #629
    ]
)


def get_this_folder():
    return pathlib.Path(__file__).parent


def get_license_header() -> str:
    with open(get_this_folder() / "license_header.txt", "r", encoding="utf-8") as f:
        return f.read()


class KWDM:
    def __init__(self, filename):
        with open(filename, encoding="utf-8") as f:
            self._data: typing.Dict = json.load(f)

    def get_keywords_list(self) -> typing.List[str]:
        return list(self._data.keys())

    def get_keyword_data_dict(self, name: str) -> typing.Dict:
        return copy.deepcopy(self[name])

    def __getitem__(self, name: str) -> typing.Dict:
        return self._data[name]


def load_manifest(filename) -> typing.Dict:
    with open(filename) as f:
        manifest = json.load(f)
    return manifest


class AdditionalCards:
    def __init__(self, filename):
        with open(filename) as f:
            self._cards = json.load(f)

    def __getitem__(self, name):
        """return a copy of the additional card, since the client may mutate it."""
        return copy.deepcopy(self._cards[name])


KWDM_INSTANCE = None
MANIFEST = None
ADDITIONAL_CARDS = None

@dataclass
class Insertion:
    target_index: int = None
    target_class: str = None
    card: typing.Dict = None


def get_card(setting: typing.Dict[str, str]):
    source = setting["source"]
    if source == "kwd-data":
        data = KWDM_INSTANCE.get_keyword_data_dict(setting["keyword-name"])
        card = data[setting["card-index"]]
        return card

    if source == "additional-cards":
        return ADDITIONAL_CARDS[setting["card-name"]]

    raise Exception()


def get_classname(keyword: str):
    """convert CLASS_NAME_FOO to ClassNameFoo"""
    tokens = keyword.split("_")
    return "".join([word.title() for word in tokens])


def get_duplicate_field_names(cards) -> typing.List[str]:
    """returns names of duplicate fields in the cards or an empty list if there are none"""
    field_names = []
    for card in cards:
        for field in card["fields"]:
            field_names.append(field["name"])
    duplicates = [item for item, count in collections.Counter(field_names).items() if count > 1]
    return duplicates


def handle_reorder_cards(kwd_data, settings):
    # TODO - mark the reorders and let that get settled after the handlers run
    order = settings["order"]
    kwd_data["cards"] = [kwd_data["cards"][i] for i in order]


def handle_duplicate_cards(kwd_data, settings):
    kwd_data["duplicate"] = True
    for card_settings in settings:
        duplicate_card = kwd_data["cards"][card_settings["index"]]
        duplicate_card["duplicate"] = {
            "name": card_settings["property-name"],
            "length_func": card_settings.get("length-func", ""),
            "active_func": card_settings.get("active-func", ""),
        }


def handle_card_sets(kwd_data, settings):
    card_sets = []
    has_options = False
    for card_settings in settings:
        card_set = {
            "name": card_settings["name"],
            "source_cards": []
        }

        for card_index, source_index in enumerate(card_settings["source-indices"]):
            source_card = kwd_data["cards"][source_index]
            source_card["source_index"] = source_card["index"]
            source_card["index"] = card_index
            source_card["mark_for_removal"] = 1
            card_set["source_cards"].append(source_card)

        if "source-options" in card_settings:
            has_options = True
            for option_index in card_settings["source-options"]:
                source_option = kwd_data["options"][int(option_index)]
                option = copy.deepcopy(source_option)
                for card in option["cards"]:
                    card_index += 1
                    card["index"] = card_index
                if "options" not in card_set:
                    card_set["options"] = [option]
                else:
                    card_set["options"].extend([option])
                source_option["mark_for_removal"] = 1

        card = {
            "set": {
                "name": card_settings["name"]
            },
            "fields": [],
            "index": card_settings["target-index"],
            "target_index": card_settings["target-index"],
            "length_func": card_settings.get("length-func", ""),
            "active_func": card_settings.get("active-func", "")
        }
        insertion = Insertion(card_settings["target-index"], card_settings.get("target-name", ""), card)
        kwd_data["card_insertions"].append(insertion)
        card_sets.append(card_set)
    kwd_data["card_sets"] = {"sets": card_sets, "options": has_options}


def handle_replace_cards(kwd_data, settings):
    for card_settings in settings:
        index = card_settings["index"]
        replacement = get_card(card_settings["card"])
        replacement["index"] = index
        kwd_data["cards"][index] = replacement


def handle_insert_cards(kwd_data, settings):
    for card_settings in settings:
        index = card_settings["index"]
        card = get_card(card_settings["card"])
        insertion = Insertion(index, "", card)
        kwd_data["card_insertions"].append(insertion)


def handle_variable_cards(kwd_data, settings):
    kwd_data["variable"] = True
    for card_settings in settings:
        variable_card = kwd_data["cards"][card_settings["index"]]
        # use abbreviations for some fields to make the jinja template more concise
        variable_card["variable"] = {
            "name": card_settings["name"],
            "size": card_settings["card-size"],
            "width": card_settings["element-width"],
            "length_func": card_settings.get("length-func", ""),
            "active_func": card_settings.get("active-func", ""),
            "type": card_settings["type"],
            "help": card_settings["help"],
        }


def handle_conditional_cards(kwd_data, settings):
    for setting in settings:
        index = setting["index"]
        card = kwd_data["cards"][index]
        card["func"] = setting["func"]


def handle_duplicate_card_group(kwd_data, settings):
    """group them and remove the originals"""
    kwd_data["duplicate_group"] = True
    for card_settings in settings:
        indices = card_settings["indices"]
        # build the card group
        group = {
            "duplicate_group": True,
            "sub_cards": [],
            "overall_name": card_settings["overall-name"],
            "length_func": card_settings.get("length-func", ""),
        }
        for index in indices:
            sub_card = kwd_data["cards"][index]
            sub_card["mark_for_removal"] = 1
            group["sub_cards"].append(sub_card)
        # remove all the sub-cards
        indices.sort(reverse=True)
        for index in indices:
            kwd_data["cards"][index]["mark_for_removal"] = 1
        insertion = Insertion(min(indices), "", group)
        kwd_data["card_insertions"].append(insertion)


def handle_skipped_cards(kwd_data, settings):
    """skip the indices or index from settings"""
    if type(settings) == int:
        skipped_card_indices = [settings]
    else:
        skipped_card_indices = settings
    for index in skipped_card_indices:
        kwd_data["cards"][index]["mark_for_removal"] = 1


def handle_override_field(kwd_data, settings):
    """override some fields"""
    for setting in settings:
        index = setting["index"]
        name = setting["name"]
        card = kwd_data["cards"][index]
        for field in card["fields"]:
            if field["name"].lower() == name:
                if "readonly" in setting:
                    field["readonly"] = setting["readonly"]
                if "type" in setting:
                    field["type"] = setting["type"]
                if "position" in setting:
                    field["position"] = setting["position"]
                if "width" in setting:
                    field["width"] = setting["width"]
                if "default" in setting:
                    field["default"] = setting["default"]
                if "options" in setting:
                    field["options"] = setting["options"]
                if "new-name" in setting:
                    field["name"] = setting["new-name"]

def handle_rename_property(kwd_data, settings):
    for setting in settings:
        index = setting["index"]
        name = setting["name"]
        property_name = setting["property-name"]
        card = kwd_data["cards"][index]
        for field in card["fields"]:
            if field["name"].lower() == name:
                field["property_name"] = property_name


def handle_shared_field(kwd_data, settings):
    for setting in settings:
        fields = []
        for card in kwd_data["cards"]:
            for field in card["fields"]:
                if field["name"] == setting["name"]:
                    fields.append(field)
        assert len(fields) > 1
        fields[0]["card_indices"] = setting["cards"]
        for field in fields[1:]:
            field["redundant"] = True


def handle_override_subkeyword(kwd_data, settings) -> None:
    kwd_data["subkeyword"] = settings


def handle_add_option(kwd_data, settings):
    def expand(card):
        card = get_card(card)
        if "active" in card:
            card["func"] = card["active"]
        return card

    new_options = []
    for setting in settings:
        cards = [expand(card) for card in setting["cards"]]
        new_option = {
            "card_order": setting["card-order"],
            "title_order": setting["title-order"],
            "name": setting["option-name"],
            "cards": cards,
        }
        new_options.append(new_option)
    kwd_data["options"] = new_options


# functions which return a copy of keyword data after applying the handling specified by the configuration
HANDLERS = collections.OrderedDict(
    {
        "reorder-card": handle_reorder_cards,
        "duplicate-card": handle_duplicate_cards,
        "override-field": handle_override_field,
        "replace-card": handle_replace_cards,
        "insert-card": handle_insert_cards,
        "variable-card": handle_variable_cards,
        "add-option": handle_add_option,
        "card-set": handle_card_sets,
        "conditional-card": handle_conditional_cards,
        "rename-property": handle_rename_property,
        "skip-card": handle_skipped_cards,
        "duplicate-card-group": handle_duplicate_card_group,
        "shared-field": handle_shared_field,
        "override-subkeyword": handle_override_subkeyword,
    }
)


def delete_marked_indices(kwd_data):
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


def add_option_indices(kwd_data):
    index = len(kwd_data["cards"])
    for options in kwd_data.get("options", []):
        for card in options["cards"]:
            card["index"] = index
        index += 1


def add_indices(kwd_data):
    # handlers might point to cards by a specific index.
    for index, card in enumerate(kwd_data["cards"]):
        card["index"] = index


def do_insertions(kwd_data):
    # [(a,b,c)] => insert b into c at index a
    insertion_targets: typing.List[typing.Tuple[int, typing.Dict, typing.List]] = []
    for insertion in kwd_data["card_insertions"]:
        insertion: Insertion = insertion
        insertion_index = insertion.target_index
        insertion_name = insertion.target_class
        insertion_card = insertion.card
        if insertion_name == "":
            # insert directly into keyword data
            container = kwd_data["cards"]
            for index, card in enumerate(container):
                card_index = card.get("source_index", card["index"])
                if card_index == insertion_index:
                    # we are inserting right before this card, store the index
                    insertion_targets.append((index, insertion_card, container))
        else:
            # insert into another card set
            card_sets = kwd_data.get("card_sets", {})
            for card_set in card_sets["sets"]:
                container = card_set["source_cards"]
                if card_set["name"] == insertion_name:
                    found = False
                    for index, card in enumerate(container):
                        if card["index"] == insertion_index:
                            found = True
                            insertion_targets.append((index, insertion_card, container))
                    if not found:
                        insertion_targets.append((len(container), insertion_card, container))
    for index, item, container in insertion_targets:
        container.insert(index, item)


def prepare_for_insertion(kwd_data):
    kwd_data["card_insertions"] = []


def after_handle(kwd_data):
    do_insertions(kwd_data)
    delete_marked_indices(kwd_data)
    add_option_indices(kwd_data)


def before_handle(kwd_data):
    add_indices(kwd_data)
    prepare_for_insertion(kwd_data)


def handle_keyword_data(kwd_data, settings):
    before_handle(kwd_data)
    # we have to iterate in the order of the handlers because right now the order still matters
    # right now this is only true for reorder_card
    for handler_name, handler_func in HANDLERS.items():
        handler_settings = settings.get(handler_name)
        if handler_settings == None:
            continue
        handler_func(kwd_data, handler_settings)
    after_handle(kwd_data)


def get_fields(card: typing.Dict) -> typing.List[typing.Dict[str, typing.Any]]:
    if "duplicate_group" in card:
        fields = []
        for sub_card in card["sub_cards"]:
            fields.extend(sub_card["fields"])
        return fields
    return card["fields"]


def transform_data(data: typing.Dict[str, typing.Any]):
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
        for field in get_fields(card):
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


def get_source_keyword(keyword, settings):
    """Get the 'source' keyword to look up in LSPP structs.  Usually
     its the keyword that its passed in, but in cases where one LSPP
    keyword is generated into multiple classes - such as for
    LOAD_SEGMENT => (LOAD_SEGMENT, LOAD_SEGMENT_ID) - this could be
    overwritten by the "source-keyword" property.
    """
    source_keyword = settings.get("source-keyword", keyword)
    return source_keyword


def set_keyword_identity(kwd_data:  typing.Dict, keyword_name: str, settings: typing.Dict) -> None:
    tokens = keyword_name.split("_")
    kwd_data["keyword"] = tokens[0]
    kwd_data["subkeyword"] = "_".join(tokens[1:])
    kwd_data["title"] = handle_single_word_keyword(keyword_name)


def get_keyword_data(keyword_name, keyword, settings):
    """Gets the keyword data dict from kwdm.  Transforms it
    based on the generation settings that are passed in, if any,
    and with default transformations that are needed to produce
    valid python code.
    """
    kwd_data = {"cards": KWDM_INSTANCE.get_keyword_data_dict(keyword)}

    set_keyword_identity(kwd_data, keyword_name, settings)

    # transformations based on generation settings
    handle_keyword_data(kwd_data, settings)

    # default transformations to a valid format we need for jinja
    transform_data(kwd_data)
    return kwd_data


def get_jinja_variable(base_variable: typing.Dict) -> typing.Dict:
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


def get_base_variable(classname: str, keyword: str, keyword_options: typing.Dict) -> typing.Dict:
    source_keyword = get_source_keyword(keyword, keyword_options)
    generation_settings = keyword_options.get("generation-options", {})
    keyword_data = get_keyword_data(keyword, source_keyword, generation_settings)
    keyword_data["classname"] = classname
    alias = get_alias(keyword)
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


def generate_class(env: Environment, lib_path: str, item: typing.Dict) -> None:
    keyword = item["name"]
    fixed_keyword = fix_keyword(keyword)
    classname = item["options"].get("classname", get_classname(fixed_keyword))
    base_variable = get_base_variable(classname, keyword, item["options"])
    jinja_variable = get_jinja_variable(base_variable)
    filename = os.path.join(lib_path, "auto", fixed_keyword.lower() + ".py")
    with open(filename, "w", encoding="utf-8") as f:
        f.write(env.get_template("keyword.j2").render(**jinja_variable))


def has_duplicate_fields(keyword: str, print_names: bool) -> bool:
    logging.info(f"checking {keyword} for duplicate fields")
    try:
        cards = KWDM_INSTANCE.get_keyword_data_dict(keyword)
    except Exception as e:
        logging.error(f"error handling keyword {keyword}")
        raise e
    duplicates = get_duplicate_field_names(cards)
    if print_names and len(duplicates) > 0:
        print(f"duplicates: {duplicates}")
    return len(duplicates) > 0


def handle_single_word_keyword(keyword: str) -> typing.Tuple[str, bool]:
    tokens = keyword.split("_")
    if len(tokens) == 2 and tokens[0] == tokens[1]:
        return tokens[0]
    return keyword


def fix_keyword(keyword: str) -> str:
    """returns a "fixed" keyword in two ways:
    - a single word keyword will be defined from the kwdm as NAME_NAME,
      and the fixed keyword is just NAME
    - some keywords are not python and filesystem friendly, for example:
      MAT_BILKHU/DUBOIS_FOAM becomes MAT_BILKHU_DUBOIS_FOAM"""
    keyword = handle_single_word_keyword(keyword)
    for bad_char in ["/", "-", " ", "(", ")"]:
        keyword = keyword.replace(bad_char, "_")
    return keyword


def generate_entry_points(env: Environment, lib_path: str, keywords_list: typing.List[typing.Dict]) -> None:
    """use templates to write keywords/type_mapping.py, keywords/__init__.py and touch keywords/auto/__init__.py"""
    license_header = get_license_header()
    keywords_lists = {"license": license_header, "keywords": keywords_list}
    with open(os.path.join(lib_path, "auto_keywords.py"), "w", encoding="utf-8") as f:
        f.write(env.get_template("importer.j2").render(**keywords_lists))

    with open(os.path.join(lib_path, "type_mapping.py"), "w", encoding="utf-8") as f:
        f.write(env.get_template("type-mapping.j2").render(**keywords_lists))

    with open(pathlib.Path(lib_path) / "auto" / "__init__.py", "w" ,encoding="utf-8") as f:
        f.write(license_header)


def get_loader():
    template_folder = get_this_folder() / "templates"
    return FileSystemLoader(str(template_folder.resolve()))


def match_wildcard(keyword, wildcard):
    assert wildcard["type"] == "prefix"
    exclusions = set(wildcard.get("exclusions", []))
    for pattern in wildcard["patterns"]:
        if keyword in exclusions:
            continue
        if keyword.startswith(f"{pattern}"):
            return True
    return False


def skip_generate_keyword_class(keyword: str) -> bool:
    global SKIPPED_KEYWORDS
    if keyword in SKIPPED_KEYWORDS:
        return True
    return False


KWD_TO_ALIAS: typing.Dict[str, str] = {}
ALIAS_TO_KWD: typing.Dict[str, str] = {}


def add_alias(keyword: str, alias: str):
    KWD_TO_ALIAS[keyword] = alias
    ALIAS_TO_KWD[alias] = keyword


def get_alias(keyword: str) -> typing.Optional[str]:
    return KWD_TO_ALIAS.get(keyword, None)


def get_aliased_by(keyword: str):
    return ALIAS_TO_KWD.get(keyword, None)


def is_aliased(keyword: str):
    return keyword in ALIAS_TO_KWD.keys()


def get_undefined_alias_keywords(keywords_list: typing.List[typing.Dict]) -> typing.List[typing.Dict]:
    undefined_aliases: typing.List[typing.Dict] = []
    for alias, kwd in ALIAS_TO_KWD.items():
        if alias not in [kwd["name"] for kwd in keywords_list]:
            fixed_keyword = fix_keyword(alias).lower()
            classname = get_classname(fixed_keyword)
            fixed_base_keyword = fix_keyword(kwd).lower()
            alias_kwd = {
                "is_autogenerated": True,
                "filename": fixed_base_keyword,
                "classname": classname,
                "title": alias
            }
            undefined_aliases.append(alias_kwd)
    return undefined_aliases


def merge_options(keyword_options: typing.Dict, generation_settings: typing.Dict) -> None:
    generation_settings = copy.deepcopy(generation_settings)
    if keyword_options == {}:
        keyword_options.update({"generation-options": generation_settings})
    else:
        generation_options: typing.Dict = keyword_options.get("generation-options", {})
        if generation_options == {}:
            generation_options.update(generation_settings)
        else:
            generation_option_keys = set(generation_options.keys())
            generation_setting_keys = set(generation_settings.keys())
            intersecting_keys = generation_option_keys & generation_setting_keys
            for intersecting_key in intersecting_keys:
                generation_optinon: typing.List = generation_options[intersecting_key]
                generation_optinon.extend(generation_settings[intersecting_key])
            difference_keys = generation_setting_keys - generation_option_keys
            for difference_key in difference_keys:
                generation_options[difference_key] = generation_settings[difference_key]

def handle_wildcards(keyword_options: typing.Dict, keyword: str) -> None:
    if skip_generate_keyword_class(keyword):
        return
    if "wildcards_handled" in keyword_options.keys():
        return
    for wildcard in MANIFEST["WILDCARDS"]:
        if match_wildcard(keyword, wildcard):
            merge_options(keyword_options, wildcard["generation-options"])
    keyword_options["wildcards_handled"] = True


def get_keyword_options(keyword: str, wildcards: bool = True) -> typing.Dict:
    """Returns the generation options of the given keyword from the manifest.  If apply_wildcards is True,
    this will return the generataion options of the keyword merged with the generation options of the
    wildard that matches this keyword, if any."""
    keyword_options = MANIFEST.get(keyword, {})
    if wildcards:
        handle_wildcards(keyword_options, keyword)
    return keyword_options


def get_keyword_item(keyword: str) -> None:
    keyword_options = get_keyword_options(keyword)
    fixed_keyword = fix_keyword(keyword).lower()
    classname = keyword_options.get("classname", get_classname(fixed_keyword))
    aliased_by = get_aliased_by(keyword)
    if aliased_by:
        filename = fix_keyword(aliased_by).lower()
    else:
        filename = fixed_keyword
    keyword_item = {
        "name": keyword,
        "title": handle_single_word_keyword(keyword),
        "classname": classname,
        "filename": filename,
        "is_autogenerated": not skip_generate_keyword_class(keyword),
    }
    return keyword_item


def get_generations(keyword: str) -> typing.List[typing.Tuple]:
    keyword_options = get_keyword_options(keyword)
    if keyword_options.get("type") != "multiple":
        return [(keyword, keyword_options)]
    generations = keyword_options.get("generations")
    result = [(gen["keyword"], gen) for gen in generations]
    return result


def add_aliases(kwd_list: typing.List[str]) -> None:
    for keyword in kwd_list:
        keyword_options = get_keyword_options(keyword, False)
        if "alias" in keyword_options:
            add_alias(keyword, keyword_options["alias"])


def get_keywords_to_generate(kwd_name: typing.Optional[str] = None) -> typing.List[typing.Dict]:
    """Get keywords to generate. If a kwd name is not none, only generate
    it and its generations."""
    keywords = []
    kwd_list = KWDM_INSTANCE.get_keywords_list()

    # first get all aliases
    add_aliases(kwd_list)

    # then get keywords to generate
    for keyword in kwd_list:
        if kwd_name != None and keyword != kwd_name:
            continue
        for keyword, keyword_options in get_generations(keyword):
            item = get_keyword_item(keyword)
            item["options"] = keyword_options
            keywords.append(item)

    return keywords


def generate_classes(lib_path: str, kwd_name: typing.Optional[str] = None) -> None:
    """Generates the keyword classes, importer, and type-mapper
    if kwd_name is not None, this only generates that particular keyword class
    """
    env = Environment(loader=get_loader(), trim_blocks=True, lstrip_blocks=True)
    if not os.path.exists(os.path.join(lib_path, "auto")):
        os.mkdir(os.path.join(lib_path, "auto"))
    keywords_list = get_keywords_to_generate(kwd_name)
    for item in keywords_list:
        name = item["name"]
        if skip_generate_keyword_class(name):
            continue
        if is_aliased(name):
            continue
        generate_class(env, lib_path, item)
    keywords_list.extend(get_undefined_alias_keywords(keywords_list))
    if kwd_name == None:
        generate_entry_points(env, lib_path, keywords_list)


def clean(output):
    """Removes the files that were be generated by this system"""
    try:
        os.remove(os.path.join(output, "auto_keywords.py"))
        os.remove(os.path.join(output, "type_mapping.py"))
        shutil.rmtree(os.path.join(output, "auto"))
        print("Cleaning successful")
    except FileNotFoundError:
        print("Cleaning failed, files not found. Might be cleaned already")

def load_inputs(this_folder, args):
    global KWDM_INSTANCE, MANIFEST, ADDITIONAL_CARDS
    if args.kwd_file == "":
        KWDM_INSTANCE = KWDM(os.path.join(this_folder, "kwd.json"))
    else:
        KWDM_INSTANCE = KWDM(args.kwd_file)
    if args.manifest == "":
        MANIFEST = load_manifest(this_folder / "manifest.json")
    else:
        MANIFEST = load_manifest(args.manifest)
    if args.additional_cards == "":
        ADDITIONAL_CARDS = AdditionalCards(this_folder / "additional-cards.json")
    else:
        ADDITIONAL_CARDS = AdditionalCards(args.additional_cards)


def run_codegen(args):
    output = args.output
    this_folder = get_this_folder()
    if args.output == "":
        output = this_folder.parent / "src" / "ansys" / "dyna" / "core" /"keywords" / "keyword_classes"
        output = str(output.resolve())
    else:
        output = args.output
    if args.clean:
        clean(output)
        return

    load_inputs(this_folder, args)
    if args.keyword == "":
        kwd = None
        print(f"Generating code for all keywords")
        generate_classes(output)
    else:
        kwd = args.keyword
        print(f"Generating code for {kwd}")
        generate_classes(output, kwd)


def parse_args():
    parser = argparse.ArgumentParser(description="Run pydyna codegen")
    parser.add_argument(
        "--output", "-o", default="", help="Output folder."
        # help="Output folder. Defaults to the location of generated code in pydyna."
    )
    parser.add_argument(
        "--clean", "-c", action="store_true", help="Wipes away the generated code in the output folder."
    )
    parser.add_argument(
        "--keyword",
        "-k",
        default="",
        help="optional - keyword for which to generate.  If not set, all keywords are generated.",
    )
    parser.add_argument(
        "--manifest",
        "-m",
        default="",
        help="Path to manifest, defaults to manifest.json in the same folder as this file.",
    )
    parser.add_argument(
        "--kwd-file", default="", help="Path to keyword file, defaults to kwd.json in the same folder as this file."
    )
    parser.add_argument(
        "--additional-cards",
        default="",
        help="Path to additional cards file, defaults to additional-cards.json in the same folder as this file.",
    )
    return parser.parse_args()


if __name__ == "__main__":
    logging.basicConfig(level=logging.WARNING)

    args = parse_args()
    run_codegen(args)
