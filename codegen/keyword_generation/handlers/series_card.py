import typing

import keyword_generation.handlers.handler_base


class SeriesCardHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        kwd_data["variable"] = True
        dataclasses = []
        for card_settings in settings:
            card_index = card_settings["index"]
            type_name = card_settings["type"]
            variable_card = kwd_data["cards"][card_index]
            if type_name == "struct":
                struct_info = card_settings["struct-info"]
                struct_name = struct_info["name"]
                dataclass = {"name": struct_name, "fields": struct_info["fields"]}
                dataclasses.append(dataclass)
                type_name = f"self.{struct_name}"

            # use abbreviations for some fields to make the jinja template more concise
            variable_card["variable"] = {
                "name": card_settings["name"],
                "size": card_settings["card-size"],
                "width": card_settings["element-width"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
                "type": type_name,
                "help": card_settings["help"],
            }
        if len(dataclasses) > 0:
            kwd_data["dataclasses"] = dataclasses

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        return
