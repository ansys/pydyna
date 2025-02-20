import typing

import keyword_generation.handlers.handler_base


class ConditionalCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        for setting in settings:
            index = setting["index"]
            card = kwd_data["cards"][index]
            card["func"] = setting["func"]

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
