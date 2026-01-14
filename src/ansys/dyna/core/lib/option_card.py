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

"""Module for optional specifications."""

import abc
import io
import typing

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.card_writer import write_cards
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.parameters import ParameterSet


class OptionSpec:
    def __init__(self, name: str, card_order: int, title_order: int):
        self._name = name
        self._card_order = card_order
        self._title_order = title_order

    @property
    def name(self) -> str:
        return self._name

    @name.setter
    def name(self, value: str) -> None:
        self._name = value

    @property
    def card_order(self) -> int:
        return self._card_order

    @card_order.setter
    def card_order(self, value: int) -> None:
        self._card_order = value

    @property
    def title_order(self) -> int:
        return self._title_order

    @title_order.setter
    def title_order(self, value: int) -> None:
        self._title_order = value

    def __repr__(self) -> str:
        return f"OptionSpec(name={self.name}, card_order={self.card_order}, title_order={self.title_order})"


class OptionCardSet(CardInterface):
    def __init__(
        self,
        option_spec: OptionSpec,
        cards: typing.List[CardInterface],
        **kwargs,
    ):
        self._keyword: typing.Any = kwargs.get("keyword", None)
        self._option_spec = option_spec
        self._cards: typing.List[CardInterface] = cards
        self._format_type: format_type = kwargs.get("format", format_type.default)

    @property
    def cards(self) -> typing.List[CardInterface]:
        return self._cards

    @property
    def option_spec(self) -> OptionSpec:
        return self._option_spec

    @property
    def name(self) -> str:
        return self._option_spec.name

    @property
    def title_order(self) -> int:
        return self._option_spec.title_order

    @property
    def card_order(self) -> int:
        return self._option_spec.card_order

    @property
    def active(self) -> bool:
        return self._keyword.is_option_active(self.name)

    @active.setter
    def active(self, value: bool) -> None:
        if value:
            self._keyword.activate_option(self.name)
        else:
            self._keyword.deactivate_option(self.name)

    @property
    def format(self) -> format_type:
        """Get the card format type."""
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        """Set the card format type."""
        self._format_type = value

    def __hash__(self):
        return hash(self.card_order)

    def __lt__(self, other: "OptionCardSet"):
        return self.card_order < other.card_order

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> bool:
        """Read from buf."""
        for card in self._cards:
            card.read(buf, parameter_set)

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
    ) -> typing.Union[str, None]:
        """Renders the card in the dyna keyword format.
        :param buf: Buffer to write to. If None, the output is returned as a string
        :param format: format_type to use. Default to standard.
        """

        def _write(buf):
            # TODO - write_cards should check the active func
            if self.active:
                write_cards(self._cards, buf, format, comment)

        return write_or_return(buf, _write)


class OptionsInterface(metaclass=abc.ABCMeta):
    """Abstract base class for option card api interface."""

    @abc.abstractmethod
    def get_option_spec(self, name: str) -> OptionSpec:
        raise NotImplementedError

    @abc.abstractmethod
    def deactivate_option(self, name: str) -> None:
        raise NotImplementedError

    @abc.abstractmethod
    def activate_option(self, name: str) -> None:
        raise NotImplementedError

    @abc.abstractmethod
    def is_option_active(self, name: str) -> bool:
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def option_specs(self) -> typing.Iterable[OptionSpec]:
        """Get the card format type."""
        raise NotImplementedError


class OptionAPI:
    """API for an individual option associated with a keyword."""

    def __init__(self, options_api: OptionsInterface, name: str):
        self._options_api = options_api
        self._name = name

    @property
    def active(self) -> bool:
        return self._options_api.is_option_active(self._name)

    @active.setter
    def active(self, value: bool) -> None:
        option_spec: OptionSpec = self._options_api.get_option_spec(self._name)

        if value:
            self._options_api.activate_option(self._name)

            # Determine if we should use cascading activation based on card order
            # If title_order exists (not None/0), use title-based mutual exclusion
            # If only card_order exists, use card_order-based logic
            if option_spec.title_order:
                # Title-based behavior: deactivate mutually exclusive options
                for any_option_spec in self._options_api.option_specs:
                    if any_option_spec.name == self._name:
                        continue
                    if (
                        any_option_spec.title_order == option_spec.title_order
                        and any_option_spec.card_order == option_spec.card_order
                    ):
                        self._options_api.deactivate_option(any_option_spec.name)
            else:
                # Card order-based logic
                current_card_order = option_spec.card_order
                for any_option_spec in self._options_api.option_specs:
                    if any_option_spec.name == self._name:
                        continue
                    if any_option_spec.title_order == 0:  # Only affect options without title_order
                        if any_option_spec.card_order == current_card_order:
                            # Same card_order: mutually exclusive
                            self._options_api.deactivate_option(any_option_spec.name)
                        elif any_option_spec.card_order < current_card_order:
                            # Lower card_order: cascading activation (prerequisite)
                            self._options_api.activate_option(any_option_spec.name)
        else:
            self._options_api.deactivate_option(self._name)


class Options:
    """Option collection associated with an options API."""

    def __init__(self, api: OptionsInterface):
        self._api = api

    def __getitem__(self, name: str) -> OptionAPI:
        """Gets the option with the given name."""
        return OptionAPI(self._api, name)

    def __repr__(self) -> str:
        option_specs = self._api.option_specs
        if len(option_specs) == 0:
            return ""
        sio = io.StringIO()
        sio.write("Options:")
        for option_spec in option_specs:
            active = self._api.is_option_active(option_spec.name)
            active_string = "active" if active else "not active"
            sio.write(f"\n    {option_spec.name} option is {active_string}.")
        return sio.getvalue()

    @property
    def api(self) -> OptionsInterface:
        return self._api
