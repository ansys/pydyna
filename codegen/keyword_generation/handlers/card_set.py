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

import copy
import typing

import keyword_generation.data_model as gen
import keyword_generation.handlers.handler_base


class CardSetHandler(keyword_generation.handlers.handler_base.KeywordHandler):
    def handle(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Transform `kwd_data` based on `settings`."""
        card_sets = []
        has_options = False
        for card_settings in settings:
            card_set = {"name": card_settings["name"], "source_cards": []}

            for card_index, source_index in enumerate(card_settings["source-indices"]):
                source_card = kwd_data["cards"][source_index]
                source_card["source_index"] = source_card["index"]
                source_card["index"] = card_index
                source_card["mark_for_removal"] = 1
                card_set["source_cards"].append(source_card)

            if "source-options" in card_settings:
                has_options = True
                for option_index in card_settings["source-options"]:
                    source_option = kwd_data["options"][int(option_index)]
                    option = copy.deepcopy(source_option)
                    for card in option["cards"]:
                        card_index += 1
                        card["index"] = card_index
                    if "options" not in card_set:
                        card_set["options"] = [option]
                    else:
                        card_set["options"].extend([option])
                    source_option["mark_for_removal"] = 1

            card = {
                "set": {"name": card_settings["name"]},
                "fields": [],
                "index": card_settings["target-index"],
                "target_index": card_settings["target-index"],
                "length_func": card_settings.get("length-func", ""),
                "active_func": card_settings.get("active-func", ""),
            }
            insertion = gen.insertion.Insertion(
                card_settings["target-index"], card_settings.get("target-name", ""), card
            )
            kwd_data["card_insertions"].append(insertion)
            card_sets.append(card_set)
        kwd_data["card_sets"] = {"sets": card_sets, "options": has_options}

    def post_process(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run after all handlers have run."""
        pass
