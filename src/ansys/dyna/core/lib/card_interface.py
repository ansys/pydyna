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

import abc
from typing import Optional, TextIO

from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.parameters import ParameterSet


class CardInterface(metaclass=abc.ABCMeta):
    """Abstract base class for all implementations of keyword cards."""

    @classmethod
    def __subclasshook__(cls, subclass):
        """
        Check if a subclass implements the required interface.

        Parameters
        ----------
        subclass : type
            The class to check.

        Returns
        -------
        bool
            True if the subclass implements 'write', 'read', and 'active' methods, otherwise False.
        """
        return (
            hasattr(subclass, "write")
            and callable(subclass.write)
            and hasattr(subclass, "read")
            and callable(subclass.read)
            and hasattr(subclass, "active")
            and callable(subclass.active)
        )

    @abc.abstractmethod
    def read(self, buf: TextIO, parameter_set: Optional[ParameterSet]) -> None:
        """
        Read the card data from an input text buffer.

        Parameters
        ----------
        buf : TextIO
            Input text buffer to read from.
        parameter_set : ParameterSet or None
            Parameter set to use for reading the card. Can be None.

        Raises
        ------
        NotImplementedError
            If the method is not implemented in the subclass.
        """
        raise NotImplementedError

    @abc.abstractmethod
    def write(self, format: Optional[format_type], buf: Optional[TextIO], comment: Optional[bool]) -> Optional[str]:
        """
        Render the card in the dyna keyword format.

        Parameters
        ----------
        format : format_type or None
            Format type to use. Defaults to standard if None.
        buf : TextIO or None
            Buffer to write to. If None, the output is returned as a string.
        comment : bool or None
            Whether to include comments in the output.

        Returns
        -------
        str or None
            The rendered card as a string, or None if written to buffer.

        Raises
        ------
        NotImplementedError
            If the method is not implemented in the subclass.
        """
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def format(self) -> format_type:
        """
        Get the card format type.

        Returns
        -------
        format_type
            The format type of the card.

        Raises
        ------
        NotImplementedError
            If the property is not implemented in the subclass.
        """
        raise NotImplementedError

    @format.setter
    @abc.abstractmethod
    def format(self, value: format_type) -> None:
        """
        Set the card format type.

        Parameters
        ----------
        value : format_type
            The format type to set.

        Raises
        ------
        NotImplementedError
            If the property setter is not implemented in the subclass.
        """
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def active(self) -> bool:
        """
        Return whether the card is active.

        Returns
        -------
        bool
            True if the card is active, otherwise False.

        Raises
        ------
        NotImplementedError
            If the property is not implemented in the subclass.
        """
        raise NotImplementedError
