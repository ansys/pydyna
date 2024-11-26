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

class LsoVariableGroup(KeywordBase):
    """DYNA LSO_VARIABLE_GROUP keyword"""

    keyword = "LSO"
    subkeyword = "VARIABLE_GROUP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "solver_name",
                        str,
                        0,
                        80,
                        kwargs.get("solver_name")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "domain_type",
                        str,
                        0,
                        80,
                        kwargs.get("domain_type", "NODE")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "group_name",
                        str,
                        0,
                        80,
                        kwargs.get("group_name")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "var_name",
                        str,
                        0,
                        80,
                        kwargs.get("var_name")
                    ),
                ],
            ),
        ]

    @property
    def solver_name(self) -> typing.Optional[str]:
        """Get or set the Name of the solver.
        """ # nopep8
        return self._cards[0].get_value("solver_name")

    @solver_name.setter
    def solver_name(self, value: str) -> None:
        self._cards[0].set_value("solver_name", value)

    @property
    def domain_type(self) -> str:
        """Get or set the One of the following must be used:
        NODE
        BEAM_ELEMENT
        SHELL_ELEMENT
        THICK_SHELL_ELEMENT
        SOLID_ELEMENT
        SEGMENT
        PART
        GLOBAL
        SURFACE_ELEMENT
        VOLUME_ELEMENT.
        """ # nopep8
        return self._cards[1].get_value("domain_type")

    @domain_type.setter
    def domain_type(self, value: str) -> None:
        if value not in ["NODE", "BEAM_ELEMENT", "SHELL_ELEMENT", "THICK_SHELL_ELEMENT", "SOLID_ELEMENT", "SEGMENT", "PART", "GLOBAL", "SURFACE_ELEMENT", "VOLUME_ELEMENT"]:
            raise Exception("""domain_type must be one of {"NODE","BEAM_ELEMENT","SHELL_ELEMENT","THICK_SHELL_ELEMENT","SOLID_ELEMENT","SEGMENT","PART","GLOBAL","SURFACE_ELEMENT","VOLUME_ELEMENT"}""")
        self._cards[1].set_value("domain_type", value)

    @property
    def group_name(self) -> typing.Optional[str]:
        """Get or set the Group name representing the list of variable_name names.
        """ # nopep8
        return self._cards[2].get_value("group_name")

    @group_name.setter
    def group_name(self, value: str) -> None:
        self._cards[2].set_value("group_name", value)

    @property
    def var_name(self) -> typing.Optional[str]:
        """Get or set the Either the name of another variable group or one of the following known edit variables:
        NODE (MECH) displacement
        BEAM (MECH)
        SHELL (MECH)
        TSHELL (MECH)
        PART (MECH)
        NODE (ICFD) temperature,
        pressure,
        species_1_density,
        species_2_density,
        enstrophy,
        helicity,
        x_velocity, y_velocity, z_velocity,
        velocity,
        x_vorticity, y_vorticity, z_vorticity,
        vorticity,	heat_flux,
        NODE (CESE)
        SURFACE_ELEMENT (ICFD)
        element_pressure
        VOLUME_ELEMENT (ICFD)
        element_pressure
        VOLUME_ELEMENT (CESE)
        density,
        temperature, pressure, internal energy,
        species_1_density, species_2_density,
        enstrophy,
        helicity,
        x_velocity, y_velocity, z_velocity,
        velocity,
        x_vorticity, y_vorticity, z_vorticity,
        vorticity,
        x_centroid, y_centroid, z_centroid,
        centroid,
        density_var,
        ddx_density,
        ddy_density,
        ddz_density,
        density,
        x_momentum_var,
        ddx_momentum,
        ddy_momentum,
        ddz_momentum,
        x_momentum,
        ddx_momentum,
        ddy_momentum,
        ddz_momentum,
        y_momentum,
        z_momentum_var,
        ddx_momentum,
        ddy_momentum,
        ddz_momentum,
        z_momentum,
        momentum,
        total_e_var,
        ddx_total_e,
        ddy_total_e,
        ddz_total_e,
        total_e,
        SEGMENT (MECH)
        SEGMENT (ICFD) drag_force_magnitude,
        ave_drag_force_magnitude,
        tot_drag_force_magnitude,
        mass_outflow,
        tot_mass_outflow,
        tot_mass-outflow_rate,
        SEGMENT (CESE)
        SURFACE_PART (ICFD)
        VOLUME_PART (ICFD)
        PART (CESE) mat_mass, mat_internal_energy
        GLOBAL (MECH)
        GLOBAL (ICFD) div(u), total_ke, total_enstrophy, total_drag
        GLOBAL (CESE) total_ke, tot_enstrophy.
        """ # nopep8
        return self._cards[3].get_value("var_name")

    @var_name.setter
    def var_name(self, value: str) -> None:
        self._cards[3].set_value("var_name", value)

