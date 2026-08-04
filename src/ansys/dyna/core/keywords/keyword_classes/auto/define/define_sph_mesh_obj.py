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

"""Module providing the DefineSphMeshObj class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINESPHMESHOBJ_CARD0 = (
    FieldSchema("filename", str, 0, 80, None),
)

_DEFINESPHMESHOBJ_CARD1 = (
    FieldSchema("ppid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("sphpid", int, 20, 10, None),
)

_DEFINESPHMESHOBJ_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineSphMeshObj(KeywordBase):
    """DYNA DEFINE_SPH_MESH_OBJ keyword"""

    keyword = "DEFINE"
    subkeyword = "SPH_MESH_OBJ"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "ppid": LinkType.PART,
        "pid": LinkType.PART,
        "sphpid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineSphMeshObj class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESPHMESHOBJ_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DEFINESPHMESHOBJ_CARD1,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = DefineSphMeshObj._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESPHMESHOBJ_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of the OBJ file to be included
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        """Set the filename property."""
        self._cards[0].set_value("filename", value)

    @property
    def ppid(self) -> typing.Optional[int]:
        """Get or set the Parent part ID. The OBJ geometry will follow the motion of the rigid part with ID PPID.
        """ # nopep8
        return self._cards[1].get_value("ppid")

    @ppid.setter
    def ppid(self, value: int) -> None:
        """Set the ppid property."""
        self._cards[1].set_value("ppid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Optional part ID to identify this OBJ file in d3plot and ISPHFOR outputs. An ID will be automatically generated if this parameter is left blank.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def sphpid(self) -> typing.Optional[int]:
        """Get or set the SPH part ID. SPH particles from this part will follow the motion of the rigid part with ID PPID
        """ # nopep8
        return self._cards[1].get_value("sphpid")

    @sphpid.setter
    def sphpid(self, value: int) -> None:
        """Set the sphpid property."""
        self._cards[1].set_value("sphpid", value)

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
    def ppid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given ppid."""
        return self._get_link_by_attr("PART", "pid", self.ppid, "parts")

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

    @property
    def sphpid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given sphpid."""
        return self._get_link_by_attr("PART", "pid", self.sphpid, "parts")

