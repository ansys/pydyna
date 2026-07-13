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

"""Module providing the EfvEos013 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOS013_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("iulctype", int, 10, 10, 0),
)

_EFVEOS013_CARD1 = (
    FieldSchema("rhoc1", float, 0, 10, None),
    FieldSchema("rhoc2", float, 10, 10, None),
    FieldSchema("rhoc3", float, 20, 10, None),
    FieldSchema("rhoc4", float, 30, 10, None),
    FieldSchema("rhoc5", float, 40, 10, None),
    FieldSchema("rhoc6", float, 50, 10, None),
    FieldSchema("rhoc7", float, 60, 10, None),
    FieldSchema("rhoc8", float, 70, 10, None),
)

_EFVEOS013_CARD2 = (
    FieldSchema("rhoc9", float, 0, 10, None),
    FieldSchema("rhoc10", float, 10, 10, None),
)

_EFVEOS013_CARD3 = (
    FieldSchema("pc1", float, 0, 10, None),
    FieldSchema("pc2", float, 10, 10, None),
    FieldSchema("pc3", float, 20, 10, None),
    FieldSchema("pc4", float, 30, 10, None),
    FieldSchema("pc5", float, 40, 10, None),
    FieldSchema("pc6", float, 50, 10, None),
    FieldSchema("pc7", float, 60, 10, None),
    FieldSchema("pc8", float, 70, 10, None),
)

_EFVEOS013_CARD4 = (
    FieldSchema("pc9", float, 0, 10, None),
    FieldSchema("pc10", float, 10, 10, None),
)

_EFVEOS013_CARD5 = (
    FieldSchema("rholu1", float, 0, 10, None),
    FieldSchema("rholu2", float, 10, 10, None),
    FieldSchema("rholu3", float, 20, 10, None),
    FieldSchema("rholu4", float, 30, 10, None),
    FieldSchema("rholu5", float, 40, 10, None),
    FieldSchema("rholu6", float, 50, 10, None),
    FieldSchema("rholu7", float, 60, 10, None),
    FieldSchema("rholu8", float, 70, 10, None),
)

_EFVEOS013_CARD6 = (
    FieldSchema("rholu9", float, 0, 10, None),
    FieldSchema("rholu10", float, 10, 10, None),
)

_EFVEOS013_CARD7 = (
    FieldSchema("clu1", float, 0, 10, None),
    FieldSchema("clu2", float, 10, 10, None),
    FieldSchema("clu3", float, 20, 10, None),
    FieldSchema("clu4", float, 30, 10, None),
    FieldSchema("clu5", float, 40, 10, None),
    FieldSchema("clu6", float, 50, 10, None),
    FieldSchema("clu7", float, 60, 10, None),
    FieldSchema("clu8", float, 70, 10, None),
)

_EFVEOS013_CARD8 = (
    FieldSchema("clu9", float, 0, 10, None),
    FieldSchema("clu10", float, 10, 10, None),
)

_EFVEOS013_CARD9 = (
    FieldSchema("rhonu1", float, 0, 10, None),
    FieldSchema("rhonu2", float, 10, 10, None),
    FieldSchema("rhonu3", float, 20, 10, None),
    FieldSchema("rhonu4", float, 30, 10, None),
    FieldSchema("rhonu5", float, 40, 10, None),
    FieldSchema("rhonu6", float, 50, 10, None),
    FieldSchema("rhonu7", float, 60, 10, None),
    FieldSchema("rhonu8", float, 70, 10, None),
)

_EFVEOS013_CARD10 = (
    FieldSchema("rhonu9", float, 0, 10, None),
    FieldSchema("rhonu10", float, 10, 10, None),
)

_EFVEOS013_CARD11 = (
    FieldSchema("knu1", float, 0, 10, None),
    FieldSchema("knu2", float, 10, 10, None),
    FieldSchema("knu3", float, 20, 10, None),
    FieldSchema("knu4", float, 30, 10, None),
    FieldSchema("knu5", float, 40, 10, None),
    FieldSchema("knu6", float, 50, 10, None),
    FieldSchema("knu7", float, 60, 10, None),
    FieldSchema("knu8", float, 70, 10, None),
)

_EFVEOS013_CARD12 = (
    FieldSchema("knu9", float, 0, 10, None),
    FieldSchema("knu10", float, 10, 10, None),
)

class EfvEos013(KeywordBase):
    """DYNA EFV_EOS_013 keyword"""

    keyword = "EFV"
    subkeyword = "EOS_013"

    def __init__(self, **kwargs):
        """Initialize the EfvEos013 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD7,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD8,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD9,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD10,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD11,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS013_CARD12,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def iulctype(self) -> int:
        """Get or set the Type of unloading/reloading curve:
        EQ.0: Linear
        EQ.1: Nonlinear
        """ # nopep8
        return self._cards[0].get_value("iulctype")

    @iulctype.setter
    def iulctype(self, value: int) -> None:
        """Set the iulctype property."""
        if value not in [0, 1, None]:
            raise Exception("""iulctype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iulctype", value)

    @property
    def rhoc1(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc1")

    @rhoc1.setter
    def rhoc1(self, value: float) -> None:
        """Set the rhoc1 property."""
        self._cards[1].set_value("rhoc1", value)

    @property
    def rhoc2(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc2")

    @rhoc2.setter
    def rhoc2(self, value: float) -> None:
        """Set the rhoc2 property."""
        self._cards[1].set_value("rhoc2", value)

    @property
    def rhoc3(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc3")

    @rhoc3.setter
    def rhoc3(self, value: float) -> None:
        """Set the rhoc3 property."""
        self._cards[1].set_value("rhoc3", value)

    @property
    def rhoc4(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc4")

    @rhoc4.setter
    def rhoc4(self, value: float) -> None:
        """Set the rhoc4 property."""
        self._cards[1].set_value("rhoc4", value)

    @property
    def rhoc5(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc5")

    @rhoc5.setter
    def rhoc5(self, value: float) -> None:
        """Set the rhoc5 property."""
        self._cards[1].set_value("rhoc5", value)

    @property
    def rhoc6(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc6")

    @rhoc6.setter
    def rhoc6(self, value: float) -> None:
        """Set the rhoc6 property."""
        self._cards[1].set_value("rhoc6", value)

    @property
    def rhoc7(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc7")

    @rhoc7.setter
    def rhoc7(self, value: float) -> None:
        """Set the rhoc7 property."""
        self._cards[1].set_value("rhoc7", value)

    @property
    def rhoc8(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[1].get_value("rhoc8")

    @rhoc8.setter
    def rhoc8(self, value: float) -> None:
        """Set the rhoc8 property."""
        self._cards[1].set_value("rhoc8", value)

    @property
    def rhoc9(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[2].get_value("rhoc9")

    @rhoc9.setter
    def rhoc9(self, value: float) -> None:
        """Set the rhoc9 property."""
        self._cards[2].set_value("rhoc9", value)

    @property
    def rhoc10(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[2].get_value("rhoc10")

    @rhoc10.setter
    def rhoc10(self, value: float) -> None:
        """Set the rhoc10 property."""
        self._cards[2].set_value("rhoc10", value)

    @property
    def pc1(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc1")

    @pc1.setter
    def pc1(self, value: float) -> None:
        """Set the pc1 property."""
        self._cards[3].set_value("pc1", value)

    @property
    def pc2(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc2")

    @pc2.setter
    def pc2(self, value: float) -> None:
        """Set the pc2 property."""
        self._cards[3].set_value("pc2", value)

    @property
    def pc3(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc3")

    @pc3.setter
    def pc3(self, value: float) -> None:
        """Set the pc3 property."""
        self._cards[3].set_value("pc3", value)

    @property
    def pc4(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc4")

    @pc4.setter
    def pc4(self, value: float) -> None:
        """Set the pc4 property."""
        self._cards[3].set_value("pc4", value)

    @property
    def pc5(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc5")

    @pc5.setter
    def pc5(self, value: float) -> None:
        """Set the pc5 property."""
        self._cards[3].set_value("pc5", value)

    @property
    def pc6(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc6")

    @pc6.setter
    def pc6(self, value: float) -> None:
        """Set the pc6 property."""
        self._cards[3].set_value("pc6", value)

    @property
    def pc7(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc7")

    @pc7.setter
    def pc7(self, value: float) -> None:
        """Set the pc7 property."""
        self._cards[3].set_value("pc7", value)

    @property
    def pc8(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[3].get_value("pc8")

    @pc8.setter
    def pc8(self, value: float) -> None:
        """Set the pc8 property."""
        self._cards[3].set_value("pc8", value)

    @property
    def pc9(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[4].get_value("pc9")

    @pc9.setter
    def pc9(self, value: float) -> None:
        """Set the pc9 property."""
        self._cards[4].set_value("pc9", value)

    @property
    def pc10(self) -> typing.Optional[float]:
        """Get or set the Pressure values giving the piecewise linear pressure as a function of density curve. This curve gives the plastic compaction path. See Remark 2
        """ # nopep8
        return self._cards[4].get_value("pc10")

    @pc10.setter
    def pc10(self, value: float) -> None:
        """Set the pc10 property."""
        self._cards[4].set_value("pc10", value)

    @property
    def rholu1(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu1")

    @rholu1.setter
    def rholu1(self, value: float) -> None:
        """Set the rholu1 property."""
        self._cards[5].set_value("rholu1", value)

    @property
    def rholu2(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu2")

    @rholu2.setter
    def rholu2(self, value: float) -> None:
        """Set the rholu2 property."""
        self._cards[5].set_value("rholu2", value)

    @property
    def rholu3(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu3")

    @rholu3.setter
    def rholu3(self, value: float) -> None:
        """Set the rholu3 property."""
        self._cards[5].set_value("rholu3", value)

    @property
    def rholu4(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu4")

    @rholu4.setter
    def rholu4(self, value: float) -> None:
        """Set the rholu4 property."""
        self._cards[5].set_value("rholu4", value)

    @property
    def rholu5(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu5")

    @rholu5.setter
    def rholu5(self, value: float) -> None:
        """Set the rholu5 property."""
        self._cards[5].set_value("rholu5", value)

    @property
    def rholu6(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu6")

    @rholu6.setter
    def rholu6(self, value: float) -> None:
        """Set the rholu6 property."""
        self._cards[5].set_value("rholu6", value)

    @property
    def rholu7(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu7")

    @rholu7.setter
    def rholu7(self, value: float) -> None:
        """Set the rholu7 property."""
        self._cards[5].set_value("rholu7", value)

    @property
    def rholu8(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[5].get_value("rholu8")

    @rholu8.setter
    def rholu8(self, value: float) -> None:
        """Set the rholu8 property."""
        self._cards[5].set_value("rholu8", value)

    @property
    def rholu9(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[6].get_value("rholu9")

    @rholu9.setter
    def rholu9(self, value: float) -> None:
        """Set the rholu9 property."""
        self._cards[6].set_value("rholu9", value)

    @property
    def rholu10(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[6].get_value("rholu10")

    @rholu10.setter
    def rholu10(self, value: float) -> None:
        """Set the rholu10 property."""
        self._cards[6].set_value("rholu10", value)

    @property
    def clu1(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu1")

    @clu1.setter
    def clu1(self, value: float) -> None:
        """Set the clu1 property."""
        self._cards[7].set_value("clu1", value)

    @property
    def clu2(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu2")

    @clu2.setter
    def clu2(self, value: float) -> None:
        """Set the clu2 property."""
        self._cards[7].set_value("clu2", value)

    @property
    def clu3(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu3")

    @clu3.setter
    def clu3(self, value: float) -> None:
        """Set the clu3 property."""
        self._cards[7].set_value("clu3", value)

    @property
    def clu4(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu4")

    @clu4.setter
    def clu4(self, value: float) -> None:
        """Set the clu4 property."""
        self._cards[7].set_value("clu4", value)

    @property
    def clu5(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu5")

    @clu5.setter
    def clu5(self, value: float) -> None:
        """Set the clu5 property."""
        self._cards[7].set_value("clu5", value)

    @property
    def clu6(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu6")

    @clu6.setter
    def clu6(self, value: float) -> None:
        """Set the clu6 property."""
        self._cards[7].set_value("clu6", value)

    @property
    def clu7(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu7")

    @clu7.setter
    def clu7(self, value: float) -> None:
        """Set the clu7 property."""
        self._cards[7].set_value("clu7", value)

    @property
    def clu8(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[7].get_value("clu8")

    @clu8.setter
    def clu8(self, value: float) -> None:
        """Set the clu8 property."""
        self._cards[7].set_value("clu8", value)

    @property
    def clu9(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[8].get_value("clu9")

    @clu9.setter
    def clu9(self, value: float) -> None:
        """Set the clu9 property."""
        self._cards[8].set_value("clu9", value)

    @property
    def clu10(self) -> typing.Optional[float]:
        """Get or set the Sound speed values giving the piecewise linear sound speed as a function of density curve. This curve gives the elastic unloading/reloading path.  See Remark 2.
        """ # nopep8
        return self._cards[8].get_value("clu10")

    @clu10.setter
    def clu10(self, value: float) -> None:
        """Set the clu10 property."""
        self._cards[8].set_value("clu10", value)

    @property
    def rhonu1(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu1")

    @rhonu1.setter
    def rhonu1(self, value: float) -> None:
        """Set the rhonu1 property."""
        self._cards[9].set_value("rhonu1", value)

    @property
    def rhonu2(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu2")

    @rhonu2.setter
    def rhonu2(self, value: float) -> None:
        """Set the rhonu2 property."""
        self._cards[9].set_value("rhonu2", value)

    @property
    def rhonu3(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu3")

    @rhonu3.setter
    def rhonu3(self, value: float) -> None:
        """Set the rhonu3 property."""
        self._cards[9].set_value("rhonu3", value)

    @property
    def rhonu4(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu4")

    @rhonu4.setter
    def rhonu4(self, value: float) -> None:
        """Set the rhonu4 property."""
        self._cards[9].set_value("rhonu4", value)

    @property
    def rhonu5(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu5")

    @rhonu5.setter
    def rhonu5(self, value: float) -> None:
        """Set the rhonu5 property."""
        self._cards[9].set_value("rhonu5", value)

    @property
    def rhonu6(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu6")

    @rhonu6.setter
    def rhonu6(self, value: float) -> None:
        """Set the rhonu6 property."""
        self._cards[9].set_value("rhonu6", value)

    @property
    def rhonu7(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu7")

    @rhonu7.setter
    def rhonu7(self, value: float) -> None:
        """Set the rhonu7 property."""
        self._cards[9].set_value("rhonu7", value)

    @property
    def rhonu8(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[9].get_value("rhonu8")

    @rhonu8.setter
    def rhonu8(self, value: float) -> None:
        """Set the rhonu8 property."""
        self._cards[9].set_value("rhonu8", value)

    @property
    def rhonu9(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[10].get_value("rhonu9")

    @rhonu9.setter
    def rhonu9(self, value: float) -> None:
        """Set the rhonu9 property."""
        self._cards[10].set_value("rhonu9", value)

    @property
    def rhonu10(self) -> typing.Optional[float]:
        """Get or set the Density values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[10].get_value("rhonu10")

    @rhonu10.setter
    def rhonu10(self, value: float) -> None:
        """Set the rhonu10 property."""
        self._cards[10].set_value("rhonu10", value)

    @property
    def knu1(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu1")

    @knu1.setter
    def knu1(self, value: float) -> None:
        """Set the knu1 property."""
        self._cards[11].set_value("knu1", value)

    @property
    def knu2(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu2")

    @knu2.setter
    def knu2(self, value: float) -> None:
        """Set the knu2 property."""
        self._cards[11].set_value("knu2", value)

    @property
    def knu3(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu3")

    @knu3.setter
    def knu3(self, value: float) -> None:
        """Set the knu3 property."""
        self._cards[11].set_value("knu3", value)

    @property
    def knu4(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu4")

    @knu4.setter
    def knu4(self, value: float) -> None:
        """Set the knu4 property."""
        self._cards[11].set_value("knu4", value)

    @property
    def knu5(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu5")

    @knu5.setter
    def knu5(self, value: float) -> None:
        """Set the knu5 property."""
        self._cards[11].set_value("knu5", value)

    @property
    def knu6(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu6")

    @knu6.setter
    def knu6(self, value: float) -> None:
        """Set the knu6 property."""
        self._cards[11].set_value("knu6", value)

    @property
    def knu7(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu7")

    @knu7.setter
    def knu7(self, value: float) -> None:
        """Set the knu7 property."""
        self._cards[11].set_value("knu7", value)

    @property
    def knu8(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[11].get_value("knu8")

    @knu8.setter
    def knu8(self, value: float) -> None:
        """Set the knu8 property."""
        self._cards[11].set_value("knu8", value)

    @property
    def knu9(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[12].get_value("knu9")

    @knu9.setter
    def knu9(self, value: float) -> None:
        """Set the knu9 property."""
        self._cards[12].set_value("knu9", value)

    @property
    def knu10(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus values giving the piecewise bulk modulus as a function of density curve. This curve gives the nonlinear unloading path. See Remark 2
        """ # nopep8
        return self._cards[12].get_value("knu10")

    @knu10.setter
    def knu10(self, value: float) -> None:
        """Set the knu10 property."""
        self._cards[12].set_value("knu10", value)

