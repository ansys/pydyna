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

"""Module providing the ContactForceTransducerPenalty class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACTFORCETRANSDUCERPENALTY_CARD0 = (
    FieldSchema("surfa", int, 0, 10, None),
    FieldSchema("surfb", int, 10, 10, None),
    FieldSchema("surfatyp", int, 20, 10, 0),
    FieldSchema("surfbtyp", int, 30, 10, 0),
    FieldSchema("saboxid", int, 40, 10, None),
    FieldSchema("sbboxid", int, 50, 10, None),
    FieldSchema("sapr", int, 60, 10, 0),
    FieldSchema("sbpr", int, 70, 10, 0),
)

_CONTACTFORCETRANSDUCERPENALTY_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_CONTACTFORCETRANSDUCERPENALTY_CARD2 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_CONTACTFORCETRANSDUCERPENALTY_OPTION0_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

class ContactForceTransducerPenalty(KeywordBase):
    """DYNA CONTACT_FORCE_TRANSDUCER_PENALTY keyword"""

    keyword = "CONTACT"
    subkeyword = "FORCE_TRANSDUCER_PENALTY"
    option_specs = [
        OptionSpec("ID", -2, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the ContactForceTransducerPenalty class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTFORCETRANSDUCERPENALTY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTFORCETRANSDUCERPENALTY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTFORCETRANSDUCERPENALTY_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = ContactForceTransducerPenalty.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _CONTACTFORCETRANSDUCERPENALTY_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def surfa(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface (see Setting the Contact Interface). See *SET_SEGMENT, *SET_NODE_OPTION, *PART, *SET_PART or *SET_SHELL_OPTION. For ERODING_SINGLE_SURFACE and ERODING_SURFACE_TO_SURFACE contact types, use either a part ID or a part set ID. For ERODING_NODES_TO_SURFACE contact, use a node set which includes all nodes that may be exposed to contact as element erosion occurs.
        EQ.0:	Includes all parts in the case of single surface contact types
        """ # nopep8
        return self._cards[0].get_value("surfa")

    @surfa.setter
    def surfa(self, value: int) -> None:
        """Set the surfa property."""
        self._cards[0].set_value("surfa", value)

    @property
    def surfb(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact (see Setting the Contact Interface).
        EQ.0:	SURFB side is not applicable for single surface contact types.
        """ # nopep8
        return self._cards[0].get_value("surfb")

    @surfb.setter
    def surfb(self, value: int) -> None:
        """Set the surfb property."""
        self._cards[0].set_value("surfb", value)

    @property
    def surfatyp(self) -> int:
        """Get or set the The ID type of SURFA:
        EQ.0: segment set ID for surface to surface contact,
        EQ.1: shell element set ID for surface to surface contact,
        EQ.2: part set ID,
        EQ.3: part ID,
        EQ.4: node set ID for node to surface contact,
        EQ.5: include all (SURFA field) is ignored,
        EQ.6: part set ID for exempted parts. All non-exempted parts are included in the contact.
        EQ.7:	Branch ID; see *SET_PART_TREE
        """ # nopep8
        return self._cards[0].get_value("surfatyp")

    @surfatyp.setter
    def surfatyp(self, value: int) -> None:
        """Set the surfatyp property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""surfatyp must be `None` or one of {0,1,2,3,4,5,6,7}.""")
        self._cards[0].set_value("surfatyp", value)

    @property
    def surfbtyp(self) -> int:
        """Get or set the ID type of SURFB:
        EQ.0: segment set ID,
        EQ.1: shell element set ID,
        EQ.2: part set ID,
        EQ.3: part ID,
        EQ.5:Include all ( SURFB Field is ignored).
        EQ.6:	Part set ID for exempted parts.  All non-exempted parts are included in the contact.
        EQ.7:	Branch ID; see *SET_PART_TREE
        """ # nopep8
        return self._cards[0].get_value("surfbtyp")

    @surfbtyp.setter
    def surfbtyp(self, value: int) -> None:
        """Set the surfbtyp property."""
        if value not in [0, 1, 2, 3, 5, 6, 7, None]:
            raise Exception("""surfbtyp must be `None` or one of {0,1,2,3,5,6,7}.""")
        self._cards[0].set_value("surfbtyp", value)

    @property
    def saboxid(self) -> typing.Optional[int]:
        """Get or set the Include in contact definition only those SURFA nodes/segments within box SABOXID (corresponding to BOXID in *DEFINE_BOX), or if SABOXID is negative, only those SURFA nodes/segments within contact volume |SABOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SABOXID can be used only if SURFATYP is set to 2, 3, or 6, that is, SURFA is a part ID or part set ID. SABOXID is not available for ERODING contact types
        """ # nopep8
        return self._cards[0].get_value("saboxid")

    @saboxid.setter
    def saboxid(self, value: int) -> None:
        """Set the saboxid property."""
        self._cards[0].set_value("saboxid", value)

    @property
    def sbboxid(self) -> typing.Optional[int]:
        """Get or set the Include in contact definition only those SURFB segments within box SBBOXID (corresponding to BOXID in *DEFINE_BOX), or if SBBOXID is negative, only those SURFB segments within contact volume |SBBOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SBBOXID can be used only if SURFBTYP is set to 2, 3, or 6, that is, SURFB is a part ID or part set ID.  SBBOXID is not available for ERODING contact types.
        """ # nopep8
        return self._cards[0].get_value("sbboxid")

    @sbboxid.setter
    def sbboxid(self, value: int) -> None:
        """Set the sbboxid property."""
        self._cards[0].set_value("sbboxid", value)

    @property
    def sapr(self) -> int:
        """Get or set the Include the SURFA side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
        EQ.0:	Do not include.
        EQ.1 : SURFA side forces included.
        EQ.2 : Same as 1 but also allows for SURFA nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
        """ # nopep8
        return self._cards[0].get_value("sapr")

    @sapr.setter
    def sapr(self, value: int) -> None:
        """Set the sapr property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sapr must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sapr", value)

    @property
    def sbpr(self) -> int:
        """Get or set the Include the SURFB side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
        EQ.0:	Do not include.
        EQ.1 : SURFB side forces included.
        EQ.2 : Same as 1, but also allows for SURFB nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
        """ # nopep8
        return self._cards[0].get_value("sbpr")

    @sbpr.setter
    def sbpr(self, value: int) -> None:
        """Set the sbpr property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""sbpr must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("sbpr", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the ID keyword option
        """ # nopep8
        return self._cards[3].cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[3].cards[0].set_value("cid", value)

        if value:
            self.activate_option("CID")

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the Interface descriptor. We suggest using unique descriptions.
        """ # nopep8
        return self._cards[3].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[3].cards[0].set_value("heading", value)

        if value:
            self.activate_option("HEADING")

