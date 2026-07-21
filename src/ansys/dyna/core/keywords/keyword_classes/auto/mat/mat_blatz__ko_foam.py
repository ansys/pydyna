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

"""Module providing the MatBlatz_KoFoam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATBLATZ_KOFOAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("g", float, 20, 10, None),
    FieldSchema("ref", float, 30, 10, 0.0),
    FieldSchema("imodel", int, 40, 10, 0),
    FieldSchema("pr", float, 50, 10, 0.25),
    FieldSchema("fmod", float, 60, 10, 0.0),
    FieldSchema("xlinv", float, 70, 10, 2.0),
)

_MATBLATZ_KOFOAM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatBlatz_KoFoam(KeywordBase):
    """DYNA MAT_BLATZ-KO_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "BLATZ-KO_FOAM"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatBlatz_KoFoam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATBLATZ_KOFOAM_CARD0,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatBlatz_KoFoam._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATBLATZ_KOFOAM_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def ref(self) -> float:
        """Get or set the Use reference geometry to initialize the stress tensor.  *INITIAL_FOAM_REFERENCE_GEOMETRY defines the reference geometry.
        EQ.0.0: Off
        EQ.1.0 : On
        """ # nopep8
        return self._cards[0].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""ref must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("ref", value)

    @property
    def imodel(self) -> int:
        """Get or set the Model selection (optional):
        EQ.0: Default foam model with a constant Poisson's ratio set to 0.25.See Remark 1.
        EQ.1 : General Blatz - Ko model with an interpolation factor set with FMOD and Poisson's ratio specified in field PR.See Remark 2.
        EQ.2 : Ciambella model(Ciambella and Saccomandi[2014]) with an interpolation factor set with FMOD and a Poisson's ratio specified with PR.See Remark 3.
        """ # nopep8
        return self._cards[0].get_value("imodel")

    @imodel.setter
    def imodel(self, value: int) -> None:
        """Set the imodel property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""imodel must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("imodel", value)

    @property
    def pr(self) -> float:
        """Get or set the Poisson's ratio for IMODEL = 1 and 2. This field is optional. Only a physically valid value, meaning -1.0 < PR < 0.5, is accepted. If this value is not given or an invalid value is specified, the default value of 0.25 is used
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def fmod(self) -> float:
        """Get or set the Interpolation factor (0.0 <= f <= 1.0) to adjust the behavior in the General Blatz-Ko (IMODEL = 1) and Ciambella (IMODEL = 2) models.  For the General Blatz-Ko model, f = 0.0 corresponds to a purely foam-like (f = 0.0) and f = 1.0 to a purely rubber-like response. See Remarks 2 and 3.
        """ # nopep8
        return self._cards[0].get_value("fmod")

    @fmod.setter
    def fmod(self, value: float) -> None:
        """Set the fmod property."""
        self._cards[0].set_value("fmod", value)

    @property
    def xlinv(self) -> float:
        """Get or set the Stretch value, λ_I, at which the material reverts from its auxetic behavior to the normal behavior for the Ciambella model. It is optional and only used by IMODEL = 2. See Remark 3
        """ # nopep8
        return self._cards[0].get_value("xlinv")

    @xlinv.setter
    def xlinv(self, value: float) -> None:
        """Set the xlinv property."""
        self._cards[0].set_value("xlinv", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

