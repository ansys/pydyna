# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

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
