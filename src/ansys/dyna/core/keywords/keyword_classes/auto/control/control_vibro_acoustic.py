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

"""Module providing the ControlVibroAcoustic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONTROLVIBROACOUSTIC_CARD0 = (
    FieldSchema("vaflag", int, 0, 10, 0),
    FieldSchema("vaprld", int, 10, 10, 0),
    FieldSchema("vastrs", int, 20, 10, 0),
    FieldSchema("vapsd", int, 30, 10, 0),
    FieldSchema("varms", int, 40, 10, 0),
    FieldSchema("vaplot", int, 50, 10, 0),
    FieldSchema("ipanelu", int, 60, 10, None),
    FieldSchema("ipanelv", int, 70, 10, None),
)

_CONTROLVIBROACOUSTIC_CARD1 = (
    FieldSchema("restart", int, 0, 10, 0),
    FieldSchema("nmodstr", int, 10, 10, None),
)

class ControlVibroAcoustic(KeywordBase):
    """DYNA CONTROL_VIBRO_ACOUSTIC keyword"""

    keyword = "CONTROL"
    subkeyword = "VIBRO_ACOUSTIC"
    _link_fields = {
        "nmodstr": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlVibroAcoustic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLVIBROACOUSTIC_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTROLVIBROACOUSTIC_CARD1,
                **kwargs,
            ),        ]
    @property
    def vaflag(self) -> int:
        """Get or set the Loading type:
        EQ.0: No vibro-acoustic structural analysis.
        EQ.1: Base acceleration.
        EQ.2: Random pressure.
        EQ.3: Plane wave.
        EQ.4: Shock.
        EQ.5: Progressive wave.
        EQ.6: Reverberant wave.
        EQ.7: Turbulent boundary layer.
        EQ.8: Nodal force.
        EQ.9: Modal stresses/strains output only.
        """ # nopep8
        return self._cards[0].get_value("vaflag")

    @vaflag.setter
    def vaflag(self, value: int) -> None:
        """Set the vaflag property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, None]:
            raise Exception("""vaflag must be `None` or one of {0,1,2,3,4,5,6,7,8,9}.""")
        self._cards[0].set_value("vaflag", value)

    @property
    def vaprld(self) -> int:
        """Get or set the Flag for including preload:
        EQ.0: No preload.
        EQ.1: Thermal preload due to temperature difference from the neutral temperature.
        EQ.2: Mechanical preload due to static pressure.
        EQ.3: Mechanical preload due to concentrated nodal force
        """ # nopep8
        return self._cards[0].get_value("vaprld")

    @vaprld.setter
    def vaprld(self, value: int) -> None:
        """Set the vaprld property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""vaprld must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("vaprld", value)

    @property
    def vastrs(self) -> int:
        """Get or set the Flag for including stress analysis:
        EQ.0: No stress analysis, only displacement analysis is requested.
        EQ.1: Both stress and displacement analyses are requested.
        """ # nopep8
        return self._cards[0].get_value("vastrs")

    @vastrs.setter
    def vastrs(self, value: int) -> None:
        """Set the vastrs property."""
        if value not in [0, 1, None]:
            raise Exception("""vastrs must be `None` or one of {0,1}.""")
        self._cards[0].set_value("vastrs", value)

    @property
    def vapsd(self) -> int:
        """Get or set the Flag for PSD output:
        EQ.0: No PSD output is requested.
        EQ.1: PSD output is requested
        """ # nopep8
        return self._cards[0].get_value("vapsd")

    @vapsd.setter
    def vapsd(self, value: int) -> None:
        """Set the vapsd property."""
        if value not in [0, 1, None]:
            raise Exception("""vapsd must be `None` or one of {0,1}.""")
        self._cards[0].set_value("vapsd", value)

    @property
    def varms(self) -> int:
        """Get or set the Flag for RMS output:
        EQ.0: No RMS output is requested.
        EQ.1: RMS output is requested.
        """ # nopep8
        return self._cards[0].get_value("varms")

    @varms.setter
    def varms(self, value: int) -> None:
        """Set the varms property."""
        if value not in [0, 1, None]:
            raise Exception("""varms must be `None` or one of {0,1}.""")
        self._cards[0].set_value("varms", value)

    @property
    def vaplot(self) -> int:
        """Get or set the Flag for PSD broadband plots:
        EQ.0: No PSD broadband plot is requested.
        EQ.1: PSD broadband plots are requested.
        """ # nopep8
        return self._cards[0].get_value("vaplot")

    @vaplot.setter
    def vaplot(self, value: int) -> None:
        """Set the vaplot property."""
        if value not in [0, 1, None]:
            raise Exception("""vaplot must be `None` or one of {0,1}.""")
        self._cards[0].set_value("vaplot", value)

    @property
    def ipanelu(self) -> typing.Optional[int]:
        """Get or set the Number of strips in U direction
        """ # nopep8
        return self._cards[0].get_value("ipanelu")

    @ipanelu.setter
    def ipanelu(self, value: int) -> None:
        """Set the ipanelu property."""
        self._cards[0].set_value("ipanelu", value)

    @property
    def ipanelv(self) -> typing.Optional[int]:
        """Get or set the Number of strips in V direction
        """ # nopep8
        return self._cards[0].get_value("ipanelv")

    @ipanelv.setter
    def ipanelv(self, value: int) -> None:
        """Set the ipanelv property."""
        self._cards[0].set_value("ipanelv", value)

    @property
    def restart(self) -> int:
        """Get or set the EQ.0: No restart will be requested. All intermediate output is deleted.
        EQ.1: Intermediate output is retained for restart.
        EQ.2: Restart based on intermediate output in last run. All intermediate output is deleted after the current run.
        EQ.3: Restart based on intermediate output in last run. All intermediate output is retained for next restart run..
        """ # nopep8
        return self._cards[1].get_value("restart")

    @restart.setter
    def restart(self, value: int) -> None:
        """Set the restart property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""restart must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("restart", value)

    @property
    def nmodstr(self) -> typing.Optional[int]:
        """Get or set the Number of modes in modal stresses/strains output.
        """ # nopep8
        return self._cards[1].get_value("nmodstr")

    @nmodstr.setter
    def nmodstr(self, value: int) -> None:
        """Set the nmodstr property."""
        self._cards[1].set_value("nmodstr", value)

    @property
    def nmodstr_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for nmodstr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.nmodstr:
                return kwd
        return None

    @nmodstr_link.setter
    def nmodstr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for nmodstr."""
        self.nmodstr = value.lcid

