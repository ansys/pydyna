
import typing

KWDM_INSTANCE = None
MANIFEST = None
ADDITIONAL_CARDS = None

def get_card(setting: typing.Dict[str, str]):
    source = setting["source"]
    if source == "kwd-data":
        data = KWDM_INSTANCE.get_keyword_data_dict(setting["keyword-name"])
        card = data[setting["card-index"]]
        return card

    if source == "additional-cards":
        return ADDITIONAL_CARDS[setting["card-name"]]

    raise Exception()
