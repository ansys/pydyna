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

class MeshSizeShape(KeywordBase):
    """DYNA MESH_SIZE_SHAPE keyword"""

    keyword = "MESH"
    subkeyword = "SIZE_SHAPE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sname",
                        str,
                        0,
                        10,
                        kwargs.get("sname", "BOX")
                    ),
                    Field(
                        "force",
                        int,
                        10,
                        10,
                        kwargs.get("force", 0)
                    ),
                    Field(
                        "method",
                        int,
                        20,
                        10,
                        kwargs.get("method", 0)
                    ),
                    Field(
                        "bt",
                        float,
                        30,
                        10,
                        kwargs.get("bt", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        40,
                        10,
                        kwargs.get("dt", 1.E12)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "msize",
                        float,
                        0,
                        10,
                        kwargs.get("msize")
                    ),
                    Field(
                        "pminx",
                        float,
                        10,
                        10,
                        kwargs.get("pminx")
                    ),
                    Field(
                        "pminy",
                        float,
                        20,
                        10,
                        kwargs.get("pminy")
                    ),
                    Field(
                        "pminz",
                        float,
                        30,
                        10,
                        kwargs.get("pminz")
                    ),
                    Field(
                        "pmaxx",
                        float,
                        40,
                        10,
                        kwargs.get("pmaxx")
                    ),
                    Field(
                        "pmaxy",
                        float,
                        50,
                        10,
                        kwargs.get("pmaxy")
                    ),
                    Field(
                        "pmaxz",
                        float,
                        60,
                        10,
                        kwargs.get("pmaxz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "msize",
                        float,
                        0,
                        10,
                        kwargs.get("msize")
                    ),
                    Field(
                        "radius",
                        float,
                        10,
                        10,
                        kwargs.get("radius")
                    ),
                    Field(
                        "centerx",
                        float,
                        20,
                        10,
                        kwargs.get("centerx")
                    ),
                    Field(
                        "centery",
                        float,
                        30,
                        10,
                        kwargs.get("centery")
                    ),
                    Field(
                        "centerz",
                        float,
                        40,
                        10,
                        kwargs.get("centerz")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "msize",
                        float,
                        0,
                        10,
                        kwargs.get("msize")
                    ),
                    Field(
                        "radius",
                        float,
                        10,
                        10,
                        kwargs.get("radius")
                    ),
                    Field(
                        "pminx",
                        float,
                        20,
                        10,
                        kwargs.get("pminx")
                    ),
                    Field(
                        "pminy",
                        float,
                        30,
                        10,
                        kwargs.get("pminy")
                    ),
                    Field(
                        "pminz",
                        float,
                        40,
                        10,
                        kwargs.get("pminz")
                    ),
                    Field(
                        "pmaxx",
                        float,
                        50,
                        10,
                        kwargs.get("pmaxx")
                    ),
                    Field(
                        "pmaxy",
                        float,
                        60,
                        10,
                        kwargs.get("pmaxy")
                    ),
                    Field(
                        "pmaxz",
                        float,
                        70,
                        10,
                        kwargs.get("pmaxz")
                    ),
                ],
            ),
        ]

    @property
    def sname(self) -> str:
        """Get or set the Shape name. Possibilities include  box,  cylinder,  pol and  sphere
        """ # nopep8
        return self._cards[0].get_value("sname")

    @sname.setter
    def sname(self, value: str) -> None:
        if value not in ["BOX", "CYLINDER", "POL", "SPHERE"]:
            raise Exception("""sname must be one of {"BOX","CYLINDER","POL","SPHERE"}""")
        self._cards[0].set_value("sname", value)

    @property
    def force(self) -> int:
        """Get or set the Force to keep the mesh size criteria even after a remeshing is done.
        EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
        EQ.1: On.
        """ # nopep8
        return self._cards[0].get_value("force")

    @force.setter
    def force(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""force must be one of {0,1}""")
        self._cards[0].set_value("force", value)

    @property
    def method(self) -> int:
        """Get or set the Force to keep the mesh size criteria even after a remeshing is done.
        EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
        EQ.1: On.
        """ # nopep8
        return self._cards[0].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""method must be one of {0,1}""")
        self._cards[0].set_value("method", value)

    @property
    def bt(self) -> float:
        """Get or set the Force to keep the mesh size criteria even after a remeshing is done.
        EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
        EQ.1: On.
        """ # nopep8
        return self._cards[0].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        self._cards[0].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Force to keep the mesh size criteria even after a remeshing is done.
        EQ.0: Off, mesh size shape will be lost if a remeshing occurs.
        EQ.1: On.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def msize(self) -> typing.Optional[float]:
        """Get or set the Mesh size that needs to be applied in the zone of the shape defined by Sname
        """ # nopep8
        return self._cards[1].get_value("msize")

    @msize.setter
    def msize(self, value: float) -> None:
        self._cards[1].set_value("msize", value)

    @property
    def pminx(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[1].get_value("pminx")

    @pminx.setter
    def pminx(self, value: float) -> None:
        self._cards[1].set_value("pminx", value)

    @property
    def pminy(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[1].get_value("pminy")

    @pminy.setter
    def pminy(self, value: float) -> None:
        self._cards[1].set_value("pminy", value)

    @property
    def pminz(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[1].get_value("pminz")

    @pminz.setter
    def pminz(self, value: float) -> None:
        self._cards[1].set_value("pminz", value)

    @property
    def pmaxx(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[1].get_value("pmaxx")

    @pmaxx.setter
    def pmaxx(self, value: float) -> None:
        self._cards[1].set_value("pmaxx", value)

    @property
    def pmaxy(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[1].get_value("pmaxy")

    @pmaxy.setter
    def pmaxy(self, value: float) -> None:
        self._cards[1].set_value("pmaxy", value)

    @property
    def pmaxz(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[1].get_value("pmaxz")

    @pmaxz.setter
    def pmaxz(self, value: float) -> None:
        self._cards[1].set_value("pmaxz", value)

    @property
    def msize(self) -> typing.Optional[float]:
        """Get or set the Mesh size that needs to be applied in the zone of the shape defined by Sname
        """ # nopep8
        return self._cards[2].get_value("msize")

    @msize.setter
    def msize(self, value: float) -> None:
        self._cards[2].set_value("msize", value)

    @property
    def radius(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere if Sname is Sphere or of the cross section disk if Sname is Cylinder
        """ # nopep8
        return self._cards[2].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[2].set_value("radius", value)

    @property
    def centerx(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the sphere center in cases where Sname is Sphere
        """ # nopep8
        return self._cards[2].get_value("centerx")

    @centerx.setter
    def centerx(self, value: float) -> None:
        self._cards[2].set_value("centerx", value)

    @property
    def centery(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the sphere center in cases where Sname is Sphere
        """ # nopep8
        return self._cards[2].get_value("centery")

    @centery.setter
    def centery(self, value: float) -> None:
        self._cards[2].set_value("centery", value)

    @property
    def centerz(self) -> typing.Optional[float]:
        """Get or set the Coordinates of the sphere center in cases where Sname is Sphere
        """ # nopep8
        return self._cards[2].get_value("centerz")

    @centerz.setter
    def centerz(self, value: float) -> None:
        self._cards[2].set_value("centerz", value)

    @property
    def msize(self) -> typing.Optional[float]:
        """Get or set the Mesh size that needs to be applied in the zone of the shape defined by Sname
        """ # nopep8
        return self._cards[3].get_value("msize")

    @msize.setter
    def msize(self, value: float) -> None:
        self._cards[3].set_value("msize", value)

    @property
    def radius(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere if Sname is Sphere or of the cross section disk if Sname is Cylinder
        """ # nopep8
        return self._cards[3].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[3].set_value("radius", value)

    @property
    def pminx(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[3].get_value("pminx")

    @pminx.setter
    def pminx(self, value: float) -> None:
        self._cards[3].set_value("pminx", value)

    @property
    def pminy(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[3].get_value("pminy")

    @pminy.setter
    def pminy(self, value: float) -> None:
        self._cards[3].set_value("pminy", value)

    @property
    def pminz(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of minimum coordinates
        """ # nopep8
        return self._cards[3].get_value("pminz")

    @pminz.setter
    def pminz(self, value: float) -> None:
        self._cards[3].set_value("pminz", value)

    @property
    def pmaxx(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[3].get_value("pmaxx")

    @pmaxx.setter
    def pmaxx(self, value: float) -> None:
        self._cards[3].set_value("pmaxx", value)

    @property
    def pmaxy(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[3].get_value("pmaxy")

    @pmaxy.setter
    def pmaxy(self, value: float) -> None:
        self._cards[3].set_value("pmaxy", value)

    @property
    def pmaxz(self) -> typing.Optional[float]:
        """Get or set the X,Y,Z for the point of maximum coordinates
        """ # nopep8
        return self._cards[3].get_value("pmaxz")

    @pmaxz.setter
    def pmaxz(self, value: float) -> None:
        self._cards[3].set_value("pmaxz", value)

