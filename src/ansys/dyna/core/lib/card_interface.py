# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

import abc
import typing

from ansys.dyna.core.lib.format_type import format_type

# TODO - implement __repr__ on all cards


class CardInterface(metaclass=abc.ABCMeta):
    """Abstract base class for all the implementations of keyword cards."""

    @classmethod
    def __subclasshook__(cls, subclass):
        return (
            hasattr(subclass, "write")
            and callable(subclass.write)
            and hasattr(subclass, "read")
            and callable(subclass.read)
        )

    @abc.abstractmethod
    def read(self, buf: typing.TextIO) -> None:
        """Reads the card data from an input text buffer."""
        raise NotImplementedError

    @abc.abstractmethod
    def write(
        self, format: typing.Optional[format_type], buf: typing.Optional[typing.TextIO], comment: typing.Optional[bool]
    ) -> typing.Union[str, None]:
        """Renders the card in the dyna keyword format.
        :param buf: Buffer to write to. If None, the output is returned as a string
        :param format: format_type to use. Default to standard.
        """
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def format(self) -> format_type:
        """Get the card format type."""
        raise NotImplementedError

    @format.setter
    @abc.abstractmethod
    def format(self, value: format_type) -> None:
        """Set the card format type."""
        raise NotImplementedError
