import typing

from keyword_generation.data_model import get_card
import keyword_generation.handlers.handler_base


class ReplaceCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        for card_settings in settings:
            index = card_settings["index"]
            replacement = get_card(card_settings["card"])
            replacement["index"] = index
            kwd_data["cards"][index] = replacement

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
