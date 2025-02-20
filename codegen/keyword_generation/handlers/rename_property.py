import typing

import keyword_generation.handlers.handler_base


class RenamePropertyHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        for setting in settings:
            index = setting["index"]
            name = setting["name"]
            property_name = setting["property-name"]
            card = kwd_data["cards"][index]
            for field in card["fields"]:
                if field["name"].lower() == name:
                    field["property_name"] = property_name

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
