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

"""Module providing the IspgControlAdaptivity class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGCONTROLADAPTIVITY_CARD0 = (
    FieldSchema("ialign", int, 0, 10, 0),
    FieldSchema("imov", int, 10, 10, 0),
    FieldSchema("ra_scl", float, 20, 10, 1.5),
    FieldSchema("rd_scl", float, 30, 10, 0.2),
    FieldSchema("ssangle", float, 40, 10, 0.524),
    FieldSchema("asangle", float, 50, 10, 0.0873),
    FieldSchema("ndiv_min", int, 60, 10, 0),
    FieldSchema("iqcp", int, 70, 10, 0),
)

_ISPGCONTROLADAPTIVITY_CARD1 = (
    FieldSchema("hmin", float, 0, 10, 0.001),
    FieldSchema("scl_ce", float, 10, 10, 1.0),
    FieldSchema("imerge", int, 20, 10, 0),
)

class IspgControlAdaptivity(KeywordBase):
    """DYNA ISPG_CONTROL_ADAPTIVITY keyword"""

    keyword = "ISPG"
    subkeyword = "CONTROL_ADAPTIVITY"

    def __init__(self, **kwargs):
        """Initialize the IspgControlAdaptivity class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGCONTROLADAPTIVITY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ISPGCONTROLADAPTIVITY_CARD1,
                **kwargs,
            ),
        ]
    @property
    def ialign(self) -> int:
        """Get or set the Flag to determine whether ISPG nodes automatically align with structural nodes and divisions in the structural segment determined using  NDIV_MIN (see Remark 1):
        EQ.0.OR.EQ.1: Automatic alignment
        NE.0.AND.NE.1: No automatic alignment
        """ # nopep8
        return self._cards[0].get_value("ialign")

    @ialign.setter
    def ialign(self, value: int) -> None:
        """Set the ialign property."""
        self._cards[0].set_value("ialign", value)

    @property
    def imov(self) -> int:
        """Get or set the Enable staggering algorithm (see Remark 2):
        EQ.0.OR.EQ.1: Enable staggering.
        NE.0.AND.NE.1: Disable staggering.
        """ # nopep8
        return self._cards[0].get_value("imov")

    @imov.setter
    def imov(self, value: int) -> None:
        """Set the imov property."""
        self._cards[0].set_value("imov", value)

    @property
    def ra_scl(self) -> float:
        """Get or set the Scale factor for determining whether to add a new ISPG node. The adaptivity algorithm adds a node if the distance between two adjacent ISPG nodes is larger than RA_SCLxD_avg,. D_avg is the original average element edge length of an ISPG part
        """ # nopep8
        return self._cards[0].get_value("ra_scl")

    @ra_scl.setter
    def ra_scl(self, value: float) -> None:
        """Set the ra_scl property."""
        self._cards[0].set_value("ra_scl", value)

    @property
    def rd_scl(self) -> float:
        """Get or set the Scale factor for determining whether to delete an ISPG node. The adaptivity algorithm deletes a node if the distance between two adjacent ISPG nodes is smaller than RD_SCLxD_avg. D_avg is the original average element edge length of an ISPG part
        """ # nopep8
        return self._cards[0].get_value("rd_scl")

    @rd_scl.setter
    def rd_scl(self, value: float) -> None:
        """Set the rd_scl property."""
        self._cards[0].set_value("rd_scl", value)

    @property
    def ssangle(self) -> float:
        """Get or set the Critical angle (unit rad) between adjacent structural segments for ISPG edges to stagger (see IMOV defined above).
        """ # nopep8
        return self._cards[0].get_value("ssangle")

    @ssangle.setter
    def ssangle(self, value: float) -> None:
        """Set the ssangle property."""
        self._cards[0].set_value("ssangle", value)

    @property
    def asangle(self) -> float:
        """Get or set the Critical angle (unit rad) of adjacent structural segments for ISPG nodes to align to structural nodes (see IALIGN defined above).
        """ # nopep8
        return self._cards[0].get_value("asangle")

    @asangle.setter
    def asangle(self, value: float) -> None:
        """Set the asangle property."""
        self._cards[0].set_value("asangle", value)

    @property
    def ndiv_min(self) -> int:
        """Get or set the Minimum number of divisions of the structural segment for ISPG node alignment (see IALIGN above). The number of divisions in the local s (nodes 1->2 and 4->3 for a 4-node segment) and t-directions (nodes 2->3 and 1->4 for a 4-node segment) for a 4-node structural segment is determined by ndiv_s=max?(ndiv_min,2 L_s?(D_avg)) and  ndiv_t=max?(ndiv_min,2 L_t?(D_avg)), where L_s=max?(L_12,L_34 ), L_t=max?(L_23,L_14 ), and D_avg is the original average element edge length of an ISPG part.  For three-node segments, the segment's maximum length is used to determine the division number.
        """ # nopep8
        return self._cards[0].get_value("ndiv_min")

    @ndiv_min.setter
    def ndiv_min(self, value: int) -> None:
        """Set the ndiv_min property."""
        self._cards[0].set_value("ndiv_min", value)

    @property
    def iqcp(self) -> int:
        """Get or set the Control the number of quadrature points in the ISPG element used for the detection of ISPG element penetration into structural elements:
        EQ.0: Use one quadrature point(center of the ISPG element) for the detection.
        EQ.1: Use 5 points for the detection.
        """ # nopep8
        return self._cards[0].get_value("iqcp")

    @iqcp.setter
    def iqcp(self, value: int) -> None:
        """Set the iqcp property."""
        self._cards[0].set_value("iqcp", value)

    @property
    def hmin(self) -> float:
        """Get or set the Minimum relative element height compared to the average element edge length D_avg. If the element's height (for the tetrahedral elements, whose 4 nodes are at the surface of the ISPG part) after adaptivity is smaller than HMINxD_avg, then this element is excluded from the ISPG part. The default value of HMIN is 0.001. We recommend values of HMIN between 0.001 and 0.05.
        """ # nopep8
        return self._cards[1].get_value("hmin")

    @hmin.setter
    def hmin(self, value: float) -> None:
        """Set the hmin property."""
        self._cards[1].set_value("hmin", value)

    @property
    def scl_ce(self) -> float:
        """Get or set the Scale factor for the element size at the regions near the contact edges (see Figure 0-1). The element size near the contact edges is SCL_CExD_avg for the refinement. Here, D_avg is the original average element edge length for the ISPG elements. The default value of SCL_CE is 1.0. If SCL_CE is smaller than 0.333, then it is reset to 0.333.
        """ # nopep8
        return self._cards[1].get_value("scl_ce")

    @scl_ce.setter
    def scl_ce(self, value: float) -> None:
        """Set the scl_ce property."""
        self._cards[1].set_value("scl_ce", value)

    @property
    def imerge(self) -> int:
        """Get or set the Flag to automatically merge parts that are sufficiently close together into one part:
        LE.0: Do not enable merging.
        GT.0: Enable merging.
        Note that for merging to work in MPP the parts for which merging is possible must be on the same processor.Use *ISPG_DEFINE_MERGER_MPP to distribute these parts to the same processor.
        """ # nopep8
        return self._cards[1].get_value("imerge")

    @imerge.setter
    def imerge(self, value: int) -> None:
        """Set the imerge property."""
        self._cards[1].set_value("imerge", value)

