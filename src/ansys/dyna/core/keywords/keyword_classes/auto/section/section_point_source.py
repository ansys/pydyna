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

"""Module providing the SectionPointSource class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_SECTIONPOINTSOURCE_CARD0 = (
    FieldSchema("secid", int, 0, 10, None),
    FieldSchema("lcidt", int, 10, 10, None),
    FieldSchema("lcidvolr", int, 20, 10, None),
    FieldSchema("lcidvel", int, 30, 10, None),
    FieldSchema("nlc001", int, 40, 10, None),
    FieldSchema("nlc002", int, 50, 10, None),
    FieldSchema("nlc003", int, 60, 10, None),
)

_SECTIONPOINTSOURCE_CARD1 = (
    FieldSchema("nodeid", int, 0, 10, None),
    FieldSchema("vecid", int, 10, 10, None),
    FieldSchema("orifarea", float, 20, 10, None),
)

_SECTIONPOINTSOURCE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SectionPointSource(KeywordBase):
    """DYNA SECTION_POINT_SOURCE keyword"""

    keyword = "SECTION"
    subkeyword = "POINT_SOURCE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SectionPointSource class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SECTIONPOINTSOURCE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SECTIONPOINTSOURCE_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SectionPointSource.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SECTIONPOINTSOURCE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        """Set the secid property."""
        self._cards[0].set_value("secid", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Temperature load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[0].set_value("lcidt", value)

    @property
    def lcidvolr(self) -> typing.Optional[int]:
        """Get or set the Relative volume load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidvolr")

    @lcidvolr.setter
    def lcidvolr(self, value: int) -> None:
        """Set the lcidvolr property."""
        self._cards[0].set_value("lcidvolr", value)

    @property
    def lcidvel(self) -> typing.Optional[int]:
        """Get or set the Inlet flow velocity load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidvel")

    @lcidvel.setter
    def lcidvel(self, value: int) -> None:
        """Set the lcidvel property."""
        self._cards[0].set_value("lcidvel", value)

    @property
    def nlc001(self) -> typing.Optional[int]:
        """Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
        """ # nopep8
        return self._cards[0].get_value("nlc001")

    @nlc001.setter
    def nlc001(self, value: int) -> None:
        """Set the nlc001 property."""
        self._cards[0].set_value("nlc001", value)

    @property
    def nlc002(self) -> typing.Optional[int]:
        """Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
        """ # nopep8
        return self._cards[0].get_value("nlc002")

    @nlc002.setter
    def nlc002(self, value: int) -> None:
        """Set the nlc002 property."""
        self._cards[0].set_value("nlc002", value)

    @property
    def nlc003(self) -> typing.Optional[int]:
        """Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
        """ # nopep8
        return self._cards[0].get_value("nlc003")

    @nlc003.setter
    def nlc003(self, value: int) -> None:
        """Set the nlc003 property."""
        self._cards[0].set_value("nlc003", value)

    @property
    def nodeid(self) -> typing.Optional[int]:
        """Get or set the Node ID defining the location of the point source.
        """ # nopep8
        return self._cards[1].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        """Set the nodeid property."""
        self._cards[1].set_value("nodeid", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the Vector ID defining the inlet flow direction in a local coordinate system defined by NID1-NID3.  If NID1-NID3 are not defined, the vector is assumed to be defined in the global coordinate system.
        """ # nopep8
        return self._cards[1].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        """Set the vecid property."""
        self._cards[1].set_value("vecid", value)

    @property
    def orifarea(self) -> typing.Optional[float]:
        """Get or set the Point source orifice area
        """ # nopep8
        return self._cards[1].get_value("orifarea")

    @orifarea.setter
    def orifarea(self, value: float) -> None:
        """Set the orifarea property."""
        self._cards[1].set_value("orifarea", value)

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

