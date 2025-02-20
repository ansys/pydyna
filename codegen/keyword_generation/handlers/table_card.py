import typing

import keyword_generation.handlers.handler_base


class TableCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        kwd_data["duplicate"] = True
        for card_settings in settings:
            duplicate_card = kwd_data["cards"][card_settings["index"]]
            duplicate_card["duplicate"] = {
                "name": card_settings["property-name"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
            }

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
