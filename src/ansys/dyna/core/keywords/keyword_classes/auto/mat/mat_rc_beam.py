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

"""Module providing the MatRcBeam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATRCBEAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("eunl", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("fc", float, 40, 10, None),
    FieldSchema("ec1", float, 50, 10, 0.0022),
    FieldSchema("ec50", float, 60, 10, None),
    FieldSchema("resid", float, 70, 10, 0.2),
)

_MATRCBEAM_CARD1 = (
    FieldSchema("ft", float, 0, 10, None),
    FieldSchema("unitc", float, 10, 10, 1.0),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("unused", float, 30, 10, None),
    FieldSchema("unused", float, 40, 10, None),
    FieldSchema("esoft", float, 50, 10, None),
    FieldSchema("lchar", float, 60, 10, None),
    FieldSchema("output", float, 70, 10, 0.0),
)

_MATRCBEAM_CARD2 = (
    FieldSchema("fracr", float, 0, 10, None),
    FieldSchema("ymreinf", float, 10, 10, None),
    FieldSchema("prreinf", float, 20, 10, None),
    FieldSchema("syreinf", float, 30, 10, None),
    FieldSchema("sureinf", float, 40, 10, None),
    FieldSchema("eshr", float, 50, 10, 0.03),
    FieldSchema("eur", float, 60, 10, 0.2),
    FieldSchema("rreinf", float, 70, 10, 4.0),
)

class MatRcBeam(KeywordBase):
    """DYNA MAT_RC_BEAM keyword"""

    keyword = "MAT"
    subkeyword = "RC_BEAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatRcBeam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATRCBEAM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATRCBEAM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATRCBEAM_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatRcBeam.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.  A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def eunl(self) -> typing.Optional[float]:
        """Get or set the Initial unloading elastic modulus .
        """ # nopep8
        return self._cards[0].get_value("eunl")

    @eunl.setter
    def eunl(self, value: float) -> None:
        """Set the eunl property."""
        self._cards[0].set_value("eunl", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Cylinder strength (stress units).
        """ # nopep8
        return self._cards[0].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        """Set the fc property."""
        self._cards[0].set_value("fc", value)

    @property
    def ec1(self) -> float:
        """Get or set the Strain at which stress FC is reached.
        """ # nopep8
        return self._cards[0].get_value("ec1")

    @ec1.setter
    def ec1(self, value: float) -> None:
        """Set the ec1 property."""
        self._cards[0].set_value("ec1", value)

    @property
    def ec50(self) -> typing.Optional[float]:
        """Get or set the Strain at which the stress has dropped to 50% FC.
        """ # nopep8
        return self._cards[0].get_value("ec50")

    @ec50.setter
    def ec50(self, value: float) -> None:
        """Set the ec50 property."""
        self._cards[0].set_value("ec50", value)

    @property
    def resid(self) -> float:
        """Get or set the Residual strength factor
        """ # nopep8
        return self._cards[0].get_value("resid")

    @resid.setter
    def resid(self, value: float) -> None:
        """Set the resid property."""
        self._cards[0].set_value("resid", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Maximum tensile stress.
        """ # nopep8
        return self._cards[1].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        """Set the ft property."""
        self._cards[1].set_value("ft", value)

    @property
    def unitc(self) -> float:
        """Get or set the Factor to convert stress units to MPa.
        """ # nopep8
        return self._cards[1].get_value("unitc")

    @unitc.setter
    def unitc(self, value: float) -> None:
        """Set the unitc property."""
        self._cards[1].set_value("unitc", value)

    @property
    def esoft(self) -> typing.Optional[float]:
        """Get or set the Slope of stress-strain curve post-cracking in tension.
        """ # nopep8
        return self._cards[1].get_value("esoft")

    @esoft.setter
    def esoft(self, value: float) -> None:
        """Set the esoft property."""
        self._cards[1].set_value("esoft", value)

    @property
    def lchar(self) -> typing.Optional[float]:
        """Get or set the Characteristic length for strain-softening behavior.
        """ # nopep8
        return self._cards[1].get_value("lchar")

    @lchar.setter
    def lchar(self, value: float) -> None:
        """Set the lchar property."""
        self._cards[1].set_value("lchar", value)

    @property
    def output(self) -> float:
        """Get or set the Output flag controlling what is written as  plastic strain
        EQ.0.0: Curvature
        EQ.1.0:  High-tide  plastic strain in reinforcement

        """ # nopep8
        return self._cards[1].get_value("output")

    @output.setter
    def output(self, value: float) -> None:
        """Set the output property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""output must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("output", value)

    @property
    def fracr(self) -> typing.Optional[float]:
        """Get or set the Fraction of reinforcement (e.g. for 1% reinforcement FRACR=0.01).
        """ # nopep8
        return self._cards[2].get_value("fracr")

    @fracr.setter
    def fracr(self, value: float) -> None:
        """Set the fracr property."""
        self._cards[2].set_value("fracr", value)

    @property
    def ymreinf(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus of reinforcement.
        """ # nopep8
        return self._cards[2].get_value("ymreinf")

    @ymreinf.setter
    def ymreinf(self, value: float) -> None:
        """Set the ymreinf property."""
        self._cards[2].set_value("ymreinf", value)

    @property
    def prreinf(self) -> typing.Optional[float]:
        """Get or set the Poisson's Ratio of reinforcement.
        """ # nopep8
        return self._cards[2].get_value("prreinf")

    @prreinf.setter
    def prreinf(self, value: float) -> None:
        """Set the prreinf property."""
        self._cards[2].set_value("prreinf", value)

    @property
    def syreinf(self) -> typing.Optional[float]:
        """Get or set the Yield stress of reinforcement.
        """ # nopep8
        return self._cards[2].get_value("syreinf")

    @syreinf.setter
    def syreinf(self, value: float) -> None:
        """Set the syreinf property."""
        self._cards[2].set_value("syreinf", value)

    @property
    def sureinf(self) -> typing.Optional[float]:
        """Get or set the Ultimate stress of reinforcement.
        """ # nopep8
        return self._cards[2].get_value("sureinf")

    @sureinf.setter
    def sureinf(self, value: float) -> None:
        """Set the sureinf property."""
        self._cards[2].set_value("sureinf", value)

    @property
    def eshr(self) -> float:
        """Get or set the Strain at which reinforcement begins to harden.
        """ # nopep8
        return self._cards[2].get_value("eshr")

    @eshr.setter
    def eshr(self, value: float) -> None:
        """Set the eshr property."""
        self._cards[2].set_value("eshr", value)

    @property
    def eur(self) -> float:
        """Get or set the Strain at which reinforcement reaches ultimate stress.
        """ # nopep8
        return self._cards[2].get_value("eur")

    @eur.setter
    def eur(self, value: float) -> None:
        """Set the eur property."""
        self._cards[2].set_value("eur", value)

    @property
    def rreinf(self) -> float:
        """Get or set the Dimensionless Ramberg-Osgood parameter r. If zero, a default value r=4.0 will be used. If set to -1, parameters will be calculated from Kent & Park formulae
        """ # nopep8
        return self._cards[2].get_value("rreinf")

    @rreinf.setter
    def rreinf(self, value: float) -> None:
        """Set the rreinf property."""
        self._cards[2].set_value("rreinf", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

