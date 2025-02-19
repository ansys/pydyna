
import typing

KWDM_INSTANCE = None
MANIFEST = None
ADDITIONAL_CARDS = None

KWD_TO_ALIAS: typing.Dict[str, str] = {}
ALIAS_TO_KWD: typing.Dict[str, str] = {}

from .insertion import Insertion

def get_card(setting: typing.Dict[str, str]):
    source = setting["source"]
    if source == "kwd-data":
        data = KWDM_INSTANCE.get_keyword_data_dict(setting["keyword-name"])
        card = data[setting["card-index"]]
        return card

    if source == "additional-cards":
        return ADDITIONAL_CARDS[setting["card-name"]]

    raise Exception()

def add_alias(keyword: str, alias: str):
    KWD_TO_ALIAS[keyword] = alias
    ALIAS_TO_KWD[alias] = keyword


def get_alias(keyword: str) -> typing.Optional[str]:
    return KWD_TO_ALIAS.get(keyword, None)


def get_aliased_by(keyword: str):
    return ALIAS_TO_KWD.get(keyword, None)


def is_aliased(keyword: str):
    return keyword in ALIAS_TO_KWD.keys()
