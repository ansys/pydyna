import typing

import keyword_generation.handlers.handler_base
import keyword_generation.data_model as gen


class TableCardGroupHandler(keyword_generation.handlers.handler_base.KeywordHandler):

    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        kwd_data["duplicate_group"] = True
        for card_settings in settings:
            indices = card_settings["indices"]
            # build the card group
            group = {
                "duplicate_group": True,
                "sub_cards": [],
                "overall_name": card_settings["overall-name"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
            }
            for index in indices:
                sub_card = kwd_data["cards"][index]
                sub_card["mark_for_removal"] = 1
                group["sub_cards"].append(sub_card)
            # remove all the sub-cards
            indices.sort(reverse=True)
            for index in indices:
                kwd_data["cards"][index]["mark_for_removal"] = 1
            insertion = gen.Insertion(min(indices), "", group)
            kwd_data["card_insertions"].append(insertion)

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
