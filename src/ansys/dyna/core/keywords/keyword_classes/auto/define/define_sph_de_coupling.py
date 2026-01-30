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

"""Module providing the DefineSphDeCoupling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINESPHDECOUPLING_CARD0 = (
    FieldSchema("did", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_DEFINESPHDECOUPLING_CARD1 = (
    FieldSchema("sphid", int, 0, 10, None),
    FieldSchema("desid", int, 10, 10, None),
    FieldSchema("sphtyp", int, 20, 10, 0),
    FieldSchema("destyp", int, 30, 10, 0),
    FieldSchema("pfact", float, 40, 10, 1.0),
    FieldSchema("dfact", float, 50, 10, 0.0),
    FieldSchema("sphbox", int, 60, 10, None),
)

_DEFINESPHDECOUPLING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSphDeCoupling(KeywordBase):
    """DYNA DEFINE_SPH_DE_COUPLING keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_DE_COUPLING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "pfact": LinkType.SECTION,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSphDeCoupling class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPHDECOUPLING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINESPHDECOUPLING_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineSphDeCoupling.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPHDECOUPLING_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def did(self) -> typing.Optional[int]:
        """Get or set the Definition ID. This must be a unique number..
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        """Set the did property."""
        self._cards[0].set_value("did", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Definition descriptor. It is suggested that unique descriptions be	used.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def sphid(self) -> typing.Optional[int]:
        """Get or set the SPH part or part set ID.
        """ # nopep8
        return self._cards[1].get_value("sphid")

    @sphid.setter
    def sphid(self, value: int) -> None:
        """Set the sphid property."""
        self._cards[1].set_value("sphid", value)

    @property
    def desid(self) -> typing.Optional[int]:
        """Get or set the DES part or part set ID
        """ # nopep8
        return self._cards[1].get_value("desid")

    @desid.setter
    def desid(self, value: int) -> None:
        """Set the desid property."""
        self._cards[1].set_value("desid", value)

    @property
    def sphtyp(self) -> int:
        """Get or set the SPH part type:
        EQ.0: Part set ID,
        EQ.1: Part ID
        """ # nopep8
        return self._cards[1].get_value("sphtyp")

    @sphtyp.setter
    def sphtyp(self, value: int) -> None:
        """Set the sphtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""sphtyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("sphtyp", value)

    @property
    def destyp(self) -> int:
        """Get or set the DES part type:
        EQ.0: Part set ID,
        EQ.1: Part ID.
        """ # nopep8
        return self._cards[1].get_value("destyp")

    @destyp.setter
    def destyp(self, value: int) -> None:
        """Set the destyp property."""
        if value not in [0, 1, None]:
            raise Exception("""destyp must be `None` or one of {0,1}.""")
        self._cards[1].set_value("destyp", value)

    @property
    def pfact(self) -> float:
        """Get or set the Penalty scale factor.
        """ # nopep8
        return self._cards[1].get_value("pfact")

    @pfact.setter
    def pfact(self, value: float) -> None:
        """Set the pfact property."""
        self._cards[1].set_value("pfact", value)

    @property
    def dfact(self) -> float:
        """Get or set the Penalty scale factor for contact damping coefficient.
        """ # nopep8
        return self._cards[1].get_value("dfact")

    @dfact.setter
    def dfact(self, value: float) -> None:
        """Set the dfact property."""
        self._cards[1].set_value("dfact", value)

    @property
    def sphbox(self) -> typing.Optional[int]:
        """Get or set the BOX ID for SPH parts, See Remarks.
        """ # nopep8
        return self._cards[1].get_value("sphbox")

    @sphbox.setter
    def sphbox(self, value: int) -> None:
        """Set the sphbox property."""
        self._cards[1].set_value("sphbox", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def pfact_link(self) -> KeywordBase:
        """Get the SECTION_* keyword for pfact."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("SECTION"):
            if kwd.secid == self.pfact:
                return kwd
        return None

    @pfact_link.setter
    def pfact_link(self, value: KeywordBase) -> None:
        """Set the SECTION_* keyword for pfact."""
        self.pfact = value.secid

