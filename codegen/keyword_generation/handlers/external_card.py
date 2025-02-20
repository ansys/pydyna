import typing

import keyword_generation.handlers.handler_base

"""
SCHEMA example:
"index": 0,
"card": {
    "source": "include_card",
    "card-name": "IncludeCard"
},
"mixin": "IncludeCardMixin"
"""


class ExternalCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        kwd_data["mixins"] = []
        kwd_data["mixin_imports"] = []
        for setting in settings:
            card_name = setting["card"]["card-name"]
            card_index = setting["index"]
            card_source = setting["card"]["source"]
            mixin_name = setting["mixin"]
            kwd_data["mixins"].append(mixin_name)
            kwd_data["mixin_imports"].append({"source": card_source, "names": [card_name, mixin_name]})
            external_card = kwd_data["cards"][card_index]
            external_card["external"] = {"name": card_name}

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
