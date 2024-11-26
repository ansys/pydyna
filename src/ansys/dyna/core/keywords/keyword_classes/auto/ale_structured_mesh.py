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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class AleStructuredMesh(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mshid",
                        int,
                        0,
                        10,
                        kwargs.get("mshid", 0)
                    ),
                    Field(
                        "dpid",
                        int,
                        10,
                        10,
                        kwargs.get("dpid")
                    ),
                    Field(
                        "nbid",
                        int,
                        20,
                        10,
                        kwargs.get("nbid", 0)
                    ),
                    Field(
                        "ebid",
                        int,
                        30,
                        10,
                        kwargs.get("ebid", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tdeath",
                        float,
                        70,
                        10,
                        kwargs.get("tdeath", 1.0E16)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cpidx",
                        int,
                        0,
                        10,
                        kwargs.get("cpidx")
                    ),
                    Field(
                        "cpidy",
                        int,
                        10,
                        10,
                        kwargs.get("cpidy")
                    ),
                    Field(
                        "cpidz",
                        int,
                        20,
                        10,
                        kwargs.get("cpidz")
                    ),
                    Field(
                        "nid0",
                        int,
                        30,
                        10,
                        kwargs.get("nid0")
                    ),
                    Field(
                        "lcsid",
                        int,
                        40,
                        10,
                        kwargs.get("lcsid")
                    ),
                ],
            ),
        ]

    @property
    def mshid(self) -> int:
        """Get or set the S-ALE Mesh ID. A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("mshid")

    @mshid.setter
    def mshid(self, value: int) -> None:
        self._cards[0].set_value("mshid", value)

    @property
    def dpid(self) -> typing.Optional[int]:
        """Get or set the Default Part ID. The elements generated will be with DPID.
        DPID refers to an empty part contains no material and used to
        reference the mesh only. This part definition is automatically
        generated during the input phase and contains no material and
        element formulation information. Please see remark 1.
        """ # nopep8
        return self._cards[0].get_value("dpid")

    @dpid.setter
    def dpid(self, value: int) -> None:
        self._cards[0].set_value("dpid", value)

    @property
    def nbid(self) -> int:
        """Get or set the Nodes are generated and assigned with node IDs starting from NBID.
        """ # nopep8
        return self._cards[0].get_value("nbid")

    @nbid.setter
    def nbid(self, value: int) -> None:
        self._cards[0].set_value("nbid", value)

    @property
    def ebid(self) -> int:
        """Get or set the Elements are generated and assigned with element IDs starting from EBID.
        """ # nopep8
        return self._cards[0].get_value("ebid")

    @ebid.setter
    def ebid(self, value: int) -> None:
        self._cards[0].set_value("ebid", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for this mesh.Please see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def cpidx(self) -> typing.Optional[int]:
        """Get or set the Control point IDs defining node ID/value pairs along each local	axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
        """ # nopep8
        return self._cards[1].get_value("cpidx")

    @cpidx.setter
    def cpidx(self, value: int) -> None:
        self._cards[1].set_value("cpidx", value)

    @property
    def cpidy(self) -> typing.Optional[int]:
        """Get or set the Control point IDs defining node ID/value pairs along each local axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
        """ # nopep8
        return self._cards[1].get_value("cpidy")

    @cpidy.setter
    def cpidy(self, value: int) -> None:
        self._cards[1].set_value("cpidy", value)

    @property
    def cpidz(self) -> typing.Optional[int]:
        """Get or set the Control point IDs defining node ID/value pairs along each local	axis. See *ALE_STRUCTURED_MESH_CONTROL_POINTS.Setting CPIDX to 0 or -1 invokes the ALE to S-ALE converter. Please see Remark 4. Note that for 2D problems CPIDZ is ignored.
        """ # nopep8
        return self._cards[1].get_value("cpidz")

    @cpidz.setter
    def cpidz(self, value: int) -> None:
        self._cards[1].set_value("cpidz", value)

    @property
    def nid0(self) -> typing.Optional[int]:
        """Get or set the NID0 specifies the mesh origin node at the input phase. Later
        during the simulation, prescribed motion applied to this node
        gives the generated mesh the translational motion.
        """ # nopep8
        return self._cards[1].get_value("nid0")

    @nid0.setter
    def nid0(self, value: int) -> None:
        self._cards[1].set_value("nid0", value)

    @property
    def lcsid(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system ID. Please see Remark 2.
        """ # nopep8
        return self._cards[1].get_value("lcsid")

    @lcsid.setter
    def lcsid(self, value: int) -> None:
        self._cards[1].set_value("lcsid", value)

