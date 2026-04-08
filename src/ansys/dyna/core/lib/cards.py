# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Base class for cards and I/O."""

import typing
import warnings

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.card_layout import get_all_cards, get_post_options_with_no_title_order
from ansys.dyna.core.lib.card_writer import write_cards
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.kwd_line_formatter import read_line
from ansys.dyna.core.lib.option_card import Options
from ansys.dyna.core.lib.parameters import ParameterSet

if typing.TYPE_CHECKING:
    from ansys.dyna.core.lib.import_handler import ImportContext


class Cards:
    """Base class for card containers with I/O."""

    def __init__(self, keyword):
        self._cards = []
        self._keyword = keyword
        self._options = Options(keyword)

    @staticmethod
    def _enrich_warning_with_context(message: str, import_context: typing.Optional["ImportContext"] = None) -> str:
        """Enrich a warning message with location information from import context.

        Parameters
        ----------
        message : str
            The original warning message.
        import_context : ImportContext, optional
            Import context with file path and line number.

        Returns
        -------
        str
            Enriched message with location prepended if context is available.
        """
        if import_context is None:
            return message

        location_parts = []
        if import_context.path is not None:
            location_parts.append(import_context.path)
        if import_context.line_number is not None:
            location_parts.append(str(import_context.line_number))

        if location_parts:
            location = ":".join(location_parts)
            return f"[{location}] {message}"
        return message

    # -- option helpers (state is owned by KeywordBase) --

    @property
    def parameter_set(self) -> typing.Optional[ParameterSet]:
        """Get the parameter set for this Cards instance.

        For KeywordBase (where self has _parameter_set): returns self._parameter_set.
        For CardSet items (where self._keyword exists): returns self._keyword._parameter_set.
        """
        if hasattr(self, "_parameter_set"):
            return self._parameter_set
        if hasattr(self._keyword, "_parameter_set"):
            return self._keyword._parameter_set
        return None

    @property
    def options(self) -> Options:
        """Gets the options_api of this keyword, if any"""
        return self._options

    def is_option_active(self, option: str) -> bool:
        """Returns True if the given option is active."""
        return option in self._active_options

    def activate_option(self, option: str) -> None:
        """Activate the given option."""
        self._active_options.add(option)

    def deactivate_option(self, option: str) -> None:
        """Deactivate the given option."""
        if option in self._active_options:
            self._active_options.remove(option)

    def _try_activate_options(self, names: typing.List[str]) -> None:
        # In LS-DYNA, '_TITLE' in the keyword name activates the 'ID' option
        # (which adds the CID + heading card) when no explicit 'TITLE' option spec
        # is defined for this keyword.
        has_title_option = any(o.name == "TITLE" for o in self.option_specs)
        if "TITLE" in names and not has_title_option:
            names = list(names) + ["ID"]
        for option in self.option_specs:
            if option.name in names:
                self.activate_option(option.name)

    def _activate_options(self, title: str) -> None:
        if self.options is None:
            return
        title_list = title.split("_")
        self._try_activate_options(title_list)

    def get_option_spec(self, name: str) -> OptionSpec:
        """Gets the option spec for the given name."""
        for option_spec in self.option_specs:
            if option_spec.name == name:
                return option_spec
        raise Exception(f"No option spec with name `{name}` found")

    @property
    def option_specs(self) -> typing.Iterable[OptionSpec]:
        """Gets all option specs for this keyword."""
        for card in self._cards:
            if hasattr(card, "option_spec"):
                option_spec = card.option_spec
                yield option_spec
            elif hasattr(card, "option_specs"):
                for option_spec in card.option_specs:
                    yield option_spec

    # end options API interface implementation
    # -- end option helpers --

    @property
    def _cards(self) -> typing.List[CardInterface]:
        """Gets the list of cards."""
        return self._base_cards

    @_cards.setter
    def _cards(self, value: typing.List[CardInterface]) -> None:
        self._base_cards = value

    def _get_all_cards(self) -> typing.List[CardInterface]:
        return get_all_cards(self._cards, self.options.api)

    def write(
        self,
        buf: typing.TextIO,
        format: format_type,
        comment: typing.Optional[bool] = True,
        retain_parameters: bool = False,
        **kwargs,
    ):
        """Writes the cards to `buf` using `format`."""
        # Use provided parameter_set/keyword_id from kwargs (for nested CardSets)
        # or compute from self (for top-level keyword)
        parameter_set = kwargs.get("parameter_set")
        keyword_id = kwargs.get("keyword_id")
        uri_prefix = kwargs.get("uri_prefix")

        if parameter_set is None and keyword_id is None:
            if retain_parameters or (
                self.parameter_set is not None and getattr(self._keyword, "deck", None) is not None
            ):
                parameter_set = self.parameter_set
                if parameter_set is not None:
                    keyword_id = str(id(self._keyword))

        write_cards(
            self._get_all_cards(),
            buf,
            format,
            comment,
            retain_parameters=retain_parameters,
            parameter_set=parameter_set,
            keyword_id=keyword_id,
            uri_prefix=uri_prefix,
        )

    def _try_read_options_with_no_title(self, buf: typing.TextIO, parameters: ParameterSet = None) -> None:
        # some cards are not active until we read.. how to handle?
        # if there are monotonically increasing options with a title order of 0
        # *AND* all active cards have been read with the maximum title order less
        # than the monotonically increasing options, then
        # the solution is to read more lines, activating one option at a time
        # they are assumed not to be activated here. when writing, they will
        # assumed to have been activated either manually or from being read.
        pos = buf.tell()
        any_options_read = False
        cards = get_post_options_with_no_title_order(self._cards)
        exit_loop = False
        while True:
            if len(cards) == 0:
                break
            linepos = buf.tell()
            _, exit_loop = read_line(buf)
            if exit_loop:
                break
            buf.seek(linepos)
            card = cards.pop(0)
            self.options.api.activate_option(card.name)
            any_options_read = True
            card.read(buf, parameters)
        if not any_options_read:
            buf.seek(pos)

    def _read_card(
        self,
        card: CardInterface,
        buf: typing.TextIO,
        parameters: ParameterSet,
        import_context: typing.Optional["ImportContext"] = None,
    ) -> bool:
        pos = buf.tell()
        read_result = card.read(buf, parameters)

        # the card is not active after reading it. THat means we should *not* read it. Rewinding back to the buffer
        # start position. In this case any warnings caused by reading the card can be ignored.
        if not card.active:
            buf.seek(pos)
        else:
            # emit warnings from reading the card, enriched with location context
            for msg in read_result.warnings:
                enriched_msg = self._enrich_warning_with_context(msg, import_context)
                warnings.warn(enriched_msg)
        return True

    def _read_data(
        self, buf: typing.TextIO, parameters: ParameterSet, import_context: typing.Optional["ImportContext"] = None
    ) -> None:
        card_index = 0
        for card in self._get_all_cards():
            if parameters is not None:
                with parameters.scope(f"card{card_index}"):
                    self._read_card(card, buf, parameters, import_context)
            else:
                self._read_card(card, buf, parameters, import_context)
            card_index += 1

        self._try_read_options_with_no_title(buf, parameters)
