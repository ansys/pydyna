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
