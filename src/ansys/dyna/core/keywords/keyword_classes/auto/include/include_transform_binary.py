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

"""Module providing the IncludeTransformBinary class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_transformation import DefineTransformation

_INCLUDETRANSFORMBINARY_CARD0 = (
    FieldSchema("filename", str, 0, 80, None),
)

_INCLUDETRANSFORMBINARY_CARD1 = (
    FieldSchema("idnoff", int, 0, 10, 0),
    FieldSchema("ideoff", int, 10, 10, 0),
    FieldSchema("idpoff", int, 20, 10, 0),
    FieldSchema("idmoff", int, 30, 10, 0),
    FieldSchema("idsoff", int, 40, 10, 0),
    FieldSchema("idfoff", int, 50, 10, 0),
    FieldSchema("iddoff", int, 60, 10, 0),
)

_INCLUDETRANSFORMBINARY_CARD2 = (
    FieldSchema("idroff", int, 0, 10, 0),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("prefix", str, 20, 10, None),
    FieldSchema("suffix", str, 30, 10, None),
)

_INCLUDETRANSFORMBINARY_CARD3 = (
    FieldSchema("fctmas", float, 0, 10, 1.0),
    FieldSchema("fcttim", float, 10, 10, 1.0),
    FieldSchema("fctlen", float, 20, 10, 1.0),
    FieldSchema("fcttem", str, 30, 10, "1.0"),
    FieldSchema("incout1", int, 40, 10, 1),
    FieldSchema("fctchg", float, 50, 10, None),
)

_INCLUDETRANSFORMBINARY_CARD4 = (
    FieldSchema("tranid", int, 0, 10, 0),
)

class IncludeTransformBinary(KeywordBase):
    """DYNA INCLUDE_TRANSFORM_BINARY keyword"""

    keyword = "INCLUDE"
    subkeyword = "TRANSFORM_BINARY"

    def __init__(self, **kwargs):
        """Initialize the IncludeTransformBinary class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INCLUDETRANSFORMBINARY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDETRANSFORMBINARY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDETRANSFORMBINARY_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDETRANSFORMBINARY_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INCLUDETRANSFORMBINARY_CARD4,
                **kwargs,
            ),        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the File name of file to be included in this keyword file.
        Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def idnoff(self) -> int:
        """Get or set the Offset to node ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idnoff")

    @idnoff.setter
    def idnoff(self, value: int) -> None:
        """Set the idnoff property."""
        self._cards[1].set_value("idnoff", value)

    @property
    def ideoff(self) -> int:
        """Get or set the Offset to element ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("ideoff")

    @ideoff.setter
    def ideoff(self, value: int) -> None:
        """Set the ideoff property."""
        self._cards[1].set_value("ideoff", value)

    @property
    def idpoff(self) -> int:
        """Get or set the Offset to part ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idpoff")

    @idpoff.setter
    def idpoff(self, value: int) -> None:
        """Set the idpoff property."""
        self._cards[1].set_value("idpoff", value)

    @property
    def idmoff(self) -> int:
        """Get or set the Offset to material ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idmoff")

    @idmoff.setter
    def idmoff(self, value: int) -> None:
        """Set the idmoff property."""
        self._cards[1].set_value("idmoff", value)

    @property
    def idsoff(self) -> int:
        """Get or set the Offset to set ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idsoff")

    @idsoff.setter
    def idsoff(self, value: int) -> None:
        """Set the idsoff property."""
        self._cards[1].set_value("idsoff", value)

    @property
    def idfoff(self) -> int:
        """Get or set the Offset to function ID, table ID, and curve ID.
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("idfoff")

    @idfoff.setter
    def idfoff(self, value: int) -> None:
        """Set the idfoff property."""
        self._cards[1].set_value("idfoff", value)

    @property
    def iddoff(self) -> int:
        """Get or set the Offset to any ID defined through *DEFINE except the FUNCTION, TABLE, and CURVE options (see IDFOFF).
        Default is set to zero.
        """ # nopep8
        return self._cards[1].get_value("iddoff")

    @iddoff.setter
    def iddoff(self, value: int) -> None:
        """Set the iddoff property."""
        self._cards[1].set_value("iddoff", value)

    @property
    def idroff(self) -> int:
        """Get or set the Used for all offsets except for those listed above.
        """ # nopep8
        return self._cards[2].get_value("idroff")

    @idroff.setter
    def idroff(self, value: int) -> None:
        """Set the idroff property."""
        self._cards[2].set_value("idroff", value)

    @property
    def prefix(self) -> typing.Optional[str]:
        """Get or set the Prefix added to the beginning of the titles/heads defined in the keywords (like *MAT, *PART, *SECTION, *DEFINE, for examples) of the included file.  A dot, "." is automatically added between the prefix and the existing title
        """ # nopep8
        return self._cards[2].get_value("prefix")

    @prefix.setter
    def prefix(self, value: str) -> None:
        """Set the prefix property."""
        self._cards[2].set_value("prefix", value)

    @property
    def suffix(self) -> typing.Optional[str]:
        """Get or set the Suffix added to the end of the titles/heads defined in the keywords of the included file.  A dot, "." is automatically added between the suffix and the existing title
        """ # nopep8
        return self._cards[2].get_value("suffix")

    @suffix.setter
    def suffix(self, value: str) -> None:
        """Set the suffix property."""
        self._cards[2].set_value("suffix", value)

    @property
    def fctmas(self) -> float:
        """Get or set the Mass transformation factor. For example, FCTMAS=1000.0 when the original mass units are in tons and the new unit is kg.
        """ # nopep8
        return self._cards[3].get_value("fctmas")

    @fctmas.setter
    def fctmas(self, value: float) -> None:
        """Set the fctmas property."""
        self._cards[3].set_value("fctmas", value)

    @property
    def fcttim(self) -> float:
        """Get or set the Time transformation factor. For example, FCTTIM=0.001 when the original time units are in milliseconds and the new time unit is seconds.
        """ # nopep8
        return self._cards[3].get_value("fcttim")

    @fcttim.setter
    def fcttim(self, value: float) -> None:
        """Set the fcttim property."""
        self._cards[3].set_value("fcttim", value)

    @property
    def fctlen(self) -> float:
        """Get or set the Length transformation factor.
        """ # nopep8
        return self._cards[3].get_value("fctlen")

    @fctlen.setter
    def fctlen(self, value: float) -> None:
        """Set the fctlen property."""
        self._cards[3].set_value("fctlen", value)

    @property
    def fcttem(self) -> str:
        """Get or set the Temperature transformation factor: F to C (Farenheit to Centigrade), C to F, F to K, K to F, and so on.
        """ # nopep8
        return self._cards[3].get_value("fcttem")

    @fcttem.setter
    def fcttem(self, value: str) -> None:
        """Set the fcttem property."""
        self._cards[3].set_value("fcttem", value)

    @property
    def incout1(self) -> int:
        """Get or set the Set to 1 for the creation of a file, DYNA.INC, which contains the transformed data. The data in this file can be used in future include files and should be checked to ensure that all the data was transformed correctly.
        """ # nopep8
        return self._cards[3].get_value("incout1")

    @incout1.setter
    def incout1(self, value: int) -> None:
        """Set the incout1 property."""
        if value not in [1, 0, None]:
            raise Exception("""incout1 must be `None` or one of {1,0}.""")
        self._cards[3].set_value("incout1", value)

    @property
    def fctchg(self) -> typing.Optional[float]:
        """Get or set the Electric charge transformation factor, currently only applied to piezoelectric material related cards, see *MAT_ADD_PZELECTRIC for details.
        """ # nopep8
        return self._cards[3].get_value("fctchg")

    @fctchg.setter
    def fctchg(self, value: float) -> None:
        """Set the fctchg property."""
        self._cards[3].set_value("fctchg", value)

    @property
    def tranid(self) -> int:
        """Get or set the Transformation ID.
        EQ.0: no tranformation will be applied.  See the input *DEFINE_TRANSFORM.
        """ # nopep8
        return self._cards[4].get_value("tranid")

    @tranid.setter
    def tranid(self, value: int) -> None:
        """Set the tranid property."""
        self._cards[4].set_value("tranid", value)

    @property
    def tranid_link(self) -> DefineTransformation:
        """Get the DefineTransformation object for tranid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TRANSFORMATION"):
            if kwd.tranid == self.tranid:
                return kwd
        return None

    @tranid_link.setter
    def tranid_link(self, value: DefineTransformation) -> None:
        """Set the DefineTransformation object for tranid."""
        self.tranid = value.tranid

