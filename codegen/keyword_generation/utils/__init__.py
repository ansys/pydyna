import pathlib
import typing


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


def get_classname(keyword: str):
    """convert CLASS_NAME_FOO to ClassNameFoo"""
    tokens = keyword.split("_")
    return "".join([word.title() for word in tokens])


def get_this_folder():
    return pathlib.Path(__file__).parent.parent.parent


def get_license_header() -> str:
    with open(get_this_folder() / "license_header.txt", "r", encoding="utf-8") as f:
        return f.read()
