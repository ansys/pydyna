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

"""Module providing the DefineDeHbond class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEDEHBOND_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("stype", int, 10, 10, 0),
    FieldSchema("hbdfm", int, 20, 10, 1),
    FieldSchema("idim", int, 30, 10, 3),
)

_DEFINEDEHBOND_CARD1 = (
    FieldSchema("pbk_sf", float, 0, 10, 1.0),
    FieldSchema("pbs_sf", float, 10, 10, 1.0),
    FieldSchema("frgk", float, 20, 10, None),
    FieldSchema("frgs", float, 30, 10, None),
    FieldSchema("bondr", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, 0.0),
    FieldSchema("dmg", float, 60, 10, 1.0),
    FieldSchema("frmdl", int, 70, 10, 1),
)

_DEFINEDEHBOND_CARD2 = (
    FieldSchema("precrk", int, 0, 10, None),
    FieldSchema("cktype", int, 10, 10, 0),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("itfid", int, 30, 10, 0),
)

_DEFINEDEHBOND_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineDeHbond(KeywordBase):
    """DYNA DEFINE_DE_HBOND keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_HBOND"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDeHbond class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEDEHBOND_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEHBOND_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEDEHBOND_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineDeHbond.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEDEHBOND_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the DES nodes
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the SID Type
        EQ.0: DES node set
        EQ.1: DES node
        EQ.2: DES part set
        EQ.3: DES part
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""stype must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("stype", value)

    @property
    def hbdfm(self) -> int:
        """Get or set the Bond formulation:
        EQ.1: (Reserved)
        EQ.2: Nonlinear heterogeneous bond formulation for fracture analysis based on the general material models defined in the material cards. DES elements with different material models can be defined within one bond.
        """ # nopep8
        return self._cards[0].get_value("hbdfm")

    @hbdfm.setter
    def hbdfm(self, value: int) -> None:
        """Set the hbdfm property."""
        if value not in [1, 2, None]:
            raise Exception("""hbdfm must be `None` or one of {1,2}.""")
        self._cards[0].set_value("hbdfm", value)

    @property
    def idim(self) -> int:
        """Get or set the Space dimension for DES bonds:
        EQ.2: for 2D plane stress problems
        EQ.3: for 3D problems.
        """ # nopep8
        return self._cards[0].get_value("idim")

    @idim.setter
    def idim(self, value: int) -> None:
        """Set the idim property."""
        if value not in [3, 2, None]:
            raise Exception("""idim must be `None` or one of {3,2}.""")
        self._cards[0].set_value("idim", value)

    @property
    def pbk_sf(self) -> float:
        """Get or set the Scale factor for volumetric stiffness of the bond.
        """ # nopep8
        return self._cards[1].get_value("pbk_sf")

    @pbk_sf.setter
    def pbk_sf(self, value: float) -> None:
        """Set the pbk_sf property."""
        self._cards[1].set_value("pbk_sf", value)

    @property
    def pbs_sf(self) -> float:
        """Get or set the Scale factor for shear stiffness of the bond.
        """ # nopep8
        return self._cards[1].get_value("pbs_sf")

    @pbs_sf.setter
    def pbs_sf(self, value: float) -> None:
        """Set the pbs_sf property."""
        self._cards[1].set_value("pbs_sf", value)

    @property
    def frgk(self) -> typing.Optional[float]:
        """Get or set the Critical fracture energy release rate for volumetric deformation due
        to the hydrostatic pressure. Special Cases:
        EQ.0: A zero value specifies an infinite energy release rate for unbreakable bonds.
        LT.0: A negative value defines the energy release rate under volumetric
        compression (i.e. positive pressure) and FRGS defined
        below is used under volumetric expansion (i.e. negative pressure).
        """ # nopep8
        return self._cards[1].get_value("frgk")

    @frgk.setter
    def frgk(self, value: float) -> None:
        """Set the frgk property."""
        self._cards[1].set_value("frgk", value)

    @property
    def frgs(self) -> typing.Optional[float]:
        """Get or set the Critical fracture energy release rate for shear deformation. Special Cases:
        EQ.0: A zero value specifies an infinite energy release rate for unbreakable bonds.
        FRGK.LT.0: See description for FRGK.
        """ # nopep8
        return self._cards[1].get_value("frgs")

    @frgs.setter
    def frgs(self, value: float) -> None:
        """Set the frgs property."""
        self._cards[1].set_value("frgs", value)

    @property
    def bondr(self) -> typing.Optional[float]:
        """Get or set the Influence radius of the DES nodes.
        """ # nopep8
        return self._cards[1].get_value("bondr")

    @bondr.setter
    def bondr(self, value: float) -> None:
        """Set the bondr property."""
        self._cards[1].set_value("bondr", value)

    @property
    def alpha(self) -> float:
        """Get or set the Numerical damping.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def dmg(self) -> float:
        """Get or set the Continuous parameter for damage model.EQ.1.0: The bond breaks if the fracture energy in the bond
        reaches the critical value. Microdamage is not calculated.
        ¦Å (0.5,1): Microdamage effects being once the fracture energy	reaches DMGxFMG[K,S]. Upon the onset of microdamage,
        the computed damage ratio will increase (monotonically) as the fracture energy grows. Bond
        weakening from microdamage is modeled by reducing the bond stiffness in proportion to the damage ratio.
        """ # nopep8
        return self._cards[1].get_value("dmg")

    @dmg.setter
    def dmg(self, value: float) -> None:
        """Set the dmg property."""
        self._cards[1].set_value("dmg", value)

    @property
    def frmdl(self) -> int:
        """Get or set the Fracture model:
        EQ.1: Fracture energy of shear deformation is calculated based on deviatoric stresses.
        EQ.2: Fracture energy of shear deformation is calculated based on deviatoric stresses, excluding the axial component (along the bond).
        EQ.3,4: Same as 1&2, respectively, but FRGK and FRGS are read
        as the total failure energy density and will be converted to the corresponding critical fracture energy release rate.
        The total failure energy density is calculated as the total area under uniaxial tension stress-strain curve.
        EQ.5,6: Same as 3&4, respectively, as FRGK and FRGS are read
        as the total failure energy density but will not be converted. Instead, the failure energy within the bond will be
        calculated. Models 1&2 are more suitable for brittle materials, and Models 5&6
        are easier for ductile materials. Models 3&4 can be used for moderately ductile fracture accordingly.
        This is the default fracture model and applied to all DES parts, even if they have different material models. More fracture models can be
        defined for different materials by specifying an interface ID (ITFID) in the optional card.
        """ # nopep8
        return self._cards[1].get_value("frmdl")

    @frmdl.setter
    def frmdl(self, value: int) -> None:
        """Set the frmdl property."""
        if value not in [1, 2, 3, 4, 5, 6, None]:
            raise Exception("""frmdl must be `None` or one of {1,2,3,4,5,6}.""")
        self._cards[1].set_value("frmdl", value)

    @property
    def precrk(self) -> typing.Optional[int]:
        """Get or set the Shell set, define 3D surfaces of the pre-crack
        """ # nopep8
        return self._cards[2].get_value("precrk")

    @precrk.setter
    def precrk(self, value: int) -> None:
        """Set the precrk property."""
        self._cards[2].set_value("precrk", value)

    @property
    def cktype(self) -> int:
        """Get or set the CKTYPE EQ. 0: Part set
        EQ. 1: Part
        """ # nopep8
        return self._cards[2].get_value("cktype")

    @cktype.setter
    def cktype(self, value: int) -> None:
        """Set the cktype property."""
        if value not in [0, 1, None]:
            raise Exception("""cktype must be `None` or one of {0,1}.""")
        self._cards[2].set_value("cktype", value)

    @property
    def itfid(self) -> int:
        """Get or set the ID of the interface *INTERFACE_DE_HBOND, which defines
        different failure models for the heterogeneous bonds within each part and between two parts respectively
        """ # nopep8
        return self._cards[2].get_value("itfid")

    @itfid.setter
    def itfid(self, value: int) -> None:
        """Set the itfid property."""
        self._cards[2].set_value("itfid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

