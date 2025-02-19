import typing

import keyword_generation.handlers.handler_base

class SkipCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):

    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        if type(settings) == int:
            skipped_card_indices = [settings]
        else:
            skipped_card_indices = settings
        for index in skipped_card_indices:
            kwd_data["cards"][index]["mark_for_removal"] = 1


    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
