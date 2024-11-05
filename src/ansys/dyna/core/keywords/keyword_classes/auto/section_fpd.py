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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SectionFpd(KeywordBase):
    """DYNA SECTION_FPD keyword"""

    keyword = "SECTION"
    subkeyword = "FPD"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "secid",
                        int,
                        0,
                        10,
                        kwargs.get("secid")
                    ),
                    Field(
                        "elform",
                        int,
                        10,
                        10,
                        kwargs.get("elform")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dx",
                        float,
                        0,
                        10,
                        kwargs.get("dx")
                    ),
                    Field(
                        "dy",
                        float,
                        10,
                        10,
                        kwargs.get("dy")
                    ),
                    Field(
                        "dz",
                        float,
                        20,
                        10,
                        kwargs.get("dz")
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "kernel",
                        int,
                        40,
                        10,
                        kwargs.get("kernel")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        float,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tstart",
                        float,
                        30,
                        10,
                        kwargs.get("tstart", 0.0)
                    ),
                    Field(
                        "dt_imp",
                        float,
                        40,
                        10,
                        kwargs.get("dt_imp")
                    ),
                    Field(
                        "dtscl",
                        float,
                        50,
                        10,
                        kwargs.get("dtscl", 0.1)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionFpd.option_specs[0],
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
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> typing.Optional[int]:
        """Get or set the Element formulation options.
        EQ.49:	Incompressible smoothed particle Galerkin formulation
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        self._cards[0].set_value("elform", value)

    @property
    def dx(self) -> typing.Optional[float]:
        """Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
        """ # nopep8
        return self._cards[1].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        self._cards[1].set_value("dx", value)

    @property
    def dy(self) -> typing.Optional[float]:
        """Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
        """ # nopep8
        return self._cards[1].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        self._cards[1].set_value("dy", value)

    @property
    def dz(self) -> typing.Optional[float]:
        """Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
        """ # nopep8
        return self._cards[1].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        self._cards[1].set_value("dz", value)

    @property
    def kernel(self) -> typing.Optional[int]:
        """Get or set the Kernel type. KERNEL=0 is for Updated Lagrangian (UL) kernel. Currently, only UL kernel is supported
        """ # nopep8
        return self._cards[1].get_value("kernel")

    @kernel.setter
    def kernel(self, value: int) -> None:
        self._cards[1].set_value("kernel", value)

    @property
    def tstart(self) -> float:
        """Get or set the Starting time for the fully implicit ISPG iterations. Before TSTART, only 10 ISPG iterations are done in each structural implicit step to guarantee the fluid moves with the solid boundaries. After TSTART, the ISPG will do a full iteration in the structural implicit step. This option is very useful for cases where the structural simulation time is very long (e.g. in seconds or minutes), while the reflow process to a steady state is very short. With this option, we can let the full ISPG iteration start from TSTART and save some computational resources
        """ # nopep8
        return self._cards[2].get_value("tstart")

    @tstart.setter
    def tstart(self, value: float) -> None:
        self._cards[2].set_value("tstart", value)

    @property
    def dt_imp(self) -> typing.Optional[float]:
        """Get or set the Reset the implicit structural time step size to DT_IMP after TSTART. Because the solder reflow process is very fast, a small implicit structural time step size is needed. Generally, the value of DT_IMP should be around 10~50 times of ISPG time step size to guarantee the convergence of the solution if the gravity-driven simulation is deployed.This field is optional
        """ # nopep8
        return self._cards[2].get_value("dt_imp")

    @dt_imp.setter
    def dt_imp(self, value: float) -> None:
        self._cards[2].set_value("dt_imp", value)

    @property
    def dtscl(self) -> float:
        """Get or set the The time step size scaling factor for ISPG iteration. We recommend a value between 0.1~0.5. Large DTSCL may cause contact detection issues.
        """ # nopep8
        return self._cards[2].get_value("dtscl")

    @dtscl.setter
    def dtscl(self, value: float) -> None:
        self._cards[2].set_value("dtscl", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

