import os
import typing

from jinja2 import Environment

import keyword_generation.data_model as data_model
from keyword_generation.utils import fix_keyword, get_license_header, get_classname

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

def _get_base_variable(get_keyword_data, classname: str, keyword: str, keyword_options: typing.Dict) -> typing.Dict:
    source_keyword = _get_source_keyword(keyword, keyword_options)
    generation_settings = keyword_options.get("generation-options", {})
    keyword_data = get_keyword_data(keyword, source_keyword, generation_settings)
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

def generate_class(get_keyword_data, env: Environment, lib_path: str, item: typing.Dict) -> None:
    keyword = item["name"]
    fixed_keyword = fix_keyword(keyword)
    classname = item["options"].get("classname", get_classname(fixed_keyword))
    base_variable = _get_base_variable(get_keyword_data, classname, keyword, item["options"])
    jinja_variable = _get_jinja_variable(base_variable)
    filename = os.path.join(lib_path, "auto", fixed_keyword.lower() + ".py")
    with open(filename, "w", encoding="utf-8") as f:
        f.write(env.get_template("keyword.j2").render(**jinja_variable))