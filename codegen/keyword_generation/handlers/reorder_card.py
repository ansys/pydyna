import typing

import keyword_generation.handlers.handler_base


class ReorderCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        # TODO - mark the reorders and let that get settled after the handlers run
        order = settings["order"]
        kwd_data["cards"] = [kwd_data["cards"][i] for i in order]

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
