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

"""Module providing the FrequencyDomainPathPartition class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_FREQUENCYDOMAINPATHPARTITION_CARD0 = (
    FieldSchema("fbeg", float, 0, 10, None),
    FieldSchema("fend", float, 10, 10, None),
    FieldSchema("filename", str, 20, 60, None),
)

class FrequencyDomainPathPartition(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_PATH_PARTITION keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_PATH_PARTITION"

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainPathPartition class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINPATHPARTITION_CARD0,
                **kwargs,
            ),        ]
    @property
    def fbeg(self) -> typing.Optional[float]:
        """Get or set the Beginning frequency for using this database.
        """ # nopep8
        return self._cards[0].get_value("fbeg")

    @fbeg.setter
    def fbeg(self, value: float) -> None:
        """Set the fbeg property."""
        self._cards[0].set_value("fbeg", value)

    @property
    def fend(self) -> typing.Optional[float]:
        """Get or set the Ending frequency for using this database.
        """ # nopep8
        return self._cards[0].get_value("fend")

    @fend.setter
    def fend(self, value: float) -> None:
        """Set the fend property."""
        self._cards[0].set_value("fend", value)

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Path and name of the file which contains modal information.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

