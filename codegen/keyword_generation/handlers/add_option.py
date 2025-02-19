import typing

from keyword_generation.data_model import get_card
import keyword_generation.handlers.handler_base

class AddOptionHandler(keyword_generation.handlers.handler_base.KeywordHandler):

    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
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

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
