import typing

import keyword_generation.handlers.handler_base


class OverrideFieldHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        for setting in settings:
            index = setting["index"]
            name = setting["name"]
            card = kwd_data["cards"][index]
            for field in card["fields"]:
                if field["name"].lower() == name:
                    if "readonly" in setting:
                        field["readonly"] = setting["readonly"]
                    if "type" in setting:
                        field["type"] = setting["type"]
                    if "position" in setting:
                        field["position"] = setting["position"]
                    if "width" in setting:
                        field["width"] = setting["width"]
                    if "default" in setting:
                        field["default"] = setting["default"]
                    if "options" in setting:
                        field["options"] = setting["options"]
                    if "new-name" in setting:
                        field["name"] = setting["new-name"]

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
