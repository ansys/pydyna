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

"""Module providing the FrequencyDomainAcousticFrequency class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_FREQUENCYDOMAINACOUSTICFREQUENCY_CARD0 = (
    FieldSchema("fmin", float, 0, 10, None),
    FieldSchema("fmax", float, 10, 10, None),
    FieldSchema("nfreq", int, 20, 10, None),
    FieldSchema("fspace", int, 30, 10, 0),
    FieldSchema("lcfreq", int, 40, 10, 0),
    FieldSchema("bias", float, 50, 10, 3.0),
    FieldSchema("spreadf", float, 60, 10, 0.1),
    FieldSchema("fractn", int, 70, 10, 3),
)

class FrequencyDomainAcousticFrequency(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_FREQUENCY keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_FREQUENCY"
    _link_fields = {
        "lcfreq": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainAcousticFrequency class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINACOUSTICFREQUENCY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def fmin(self) -> typing.Optional[float]:
        """Get or set the Minimum frequency for output.
        """ # nopep8
        return self._cards[0].get_value("fmin")

    @fmin.setter
    def fmin(self, value: float) -> None:
        """Set the fmin property."""
        self._cards[0].set_value("fmin", value)

    @property
    def fmax(self) -> typing.Optional[float]:
        """Get or set the Maximum frequency for output. Ignored if FSPACE = 6.
        """ # nopep8
        return self._cards[0].get_value("fmax")

    @fmax.setter
    def fmax(self, value: float) -> None:
        """Set the fmax property."""
        self._cards[0].set_value("fmax", value)

    @property
    def nfreq(self) -> typing.Optional[int]:
        """Get or set the Total number of frequencies for output:
        GT.0: NFREQ is the number of frequencies for the whole range.
        LT.0: | NFREQ | is the number of frequencies for each i.
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        """Set the nfreq property."""
        self._cards[0].set_value("nfreq", value)

    @property
    def fspace(self) -> int:
        """Get or set the Frequency spacing option for output (see Figure 0-1 and Remark 1):
        EQ.0: Linear
        EQ.1: Logarithmic
        EQ.2: Biased spacing(range)
        EQ.3: Eigenfrequencies only
        EQ.4: Biased spacing(eigenfrequency)
        EQ.5: Biased spacing(eigenfrequency spread)
        EQ.6: Octave frequencies starting with FMIN
        """ # nopep8
        return self._cards[0].get_value("fspace")

    @fspace.setter
    def fspace(self, value: int) -> None:
        """Set the fspace property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, None]:
            raise Exception("""fspace must be `None` or one of {0,1,2,3,4,5,6}.""")
        self._cards[0].set_value("fspace", value)

    @property
    def lcfreq(self) -> int:
        """Get or set the Load curve ID defining the frequencies for output. If defined, all other fields are ignored.
        """ # nopep8
        return self._cards[0].get_value("lcfreq")

    @lcfreq.setter
    def lcfreq(self, value: int) -> None:
        """Set the lcfreq property."""
        self._cards[0].set_value("lcfreq", value)

    @property
    def bias(self) -> float:
        """Get or set the Bias parameter, (FSPACE = 2, 4, and 5 only).
        """ # nopep8
        return self._cards[0].get_value("bias")

    @bias.setter
    def bias(self, value: float) -> None:
        """Set the bias property."""
        self._cards[0].set_value("bias", value)

    @property
    def spreadf(self) -> float:
        """Get or set the Spread ratio, (FSPACE = 5 only)
        """ # nopep8
        return self._cards[0].get_value("spreadf")

    @spreadf.setter
    def spreadf(self, value: float) -> None:
        """Set the spreadf property."""
        self._cards[0].set_value("spreadf", value)

    @property
    def fractn(self) -> int:
        """Get or set the Octave fraction, (FSPACE = 6 only). For example, FRACTN = 3 means 1/3 octave spacing. FMAX is ignored
        """ # nopep8
        return self._cards[0].get_value("fractn")

    @fractn.setter
    def fractn(self, value: int) -> None:
        """Set the fractn property."""
        self._cards[0].set_value("fractn", value)

    @property
    def lcfreq_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfreq."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfreq:
                return kwd
        return None

    @lcfreq_link.setter
    def lcfreq_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfreq."""
        self.lcfreq = value.lcid

