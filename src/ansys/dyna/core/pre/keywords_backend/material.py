# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Material keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class MaterialKeywordsMixin:
    """Mixin class providing material keyword creation methods."""

    def create_mat_elastic_plastic_thermal(
        self,
        ro: float = 0.0,
        ti: tuple = (),
        ei: tuple = (),
        pri: tuple = (),
        alphai: tuple = (),
        sigyi: tuple = (),
    ) -> int:
        """Create a MAT_ELASTIC_PLASTIC_THERMAL keyword.

        Parameters
        ----------
        ro : float
            Mass density.
        ti : tuple
            Temperature values.
        ei : tuple
            Young's modulus values.
        pri : tuple
            Poisson's ratio values.
        alphai : tuple
            Thermal expansion coefficient values.
        sigyi : tuple
            Yield stress values.

        Returns
        -------
        int
            Material ID.
        """
        from ansys.dyna.core.keywords import keywords

        mid = self.next_id("material")
        kw = keywords.MatElasticPlasticThermal()
        kw.mid = mid
        kw.ro = ro

        # Set temperature-dependent properties
        for i, (t, e, pr, alpha, sigy) in enumerate(zip(ti, ei, pri, alphai, sigyi)):
            setattr(kw, f"t{i + 1}", t)
            setattr(kw, f"e{i + 1}", e)
            setattr(kw, f"pr{i + 1}", pr)
            setattr(kw, f"alpha{i + 1}", alpha)
            setattr(kw, f"sigy{i + 1}", sigy)
            setattr(kw, f"etan{i + 1}", 0.0)  # Default etan to 0

        self._deck.append(kw)
        logger.debug(f"Created MAT_ELASTIC_PLASTIC_THERMAL with mid={mid}")
        return mid

    def create_mat_thermal_isotropic(
        self,
        tmid: int = 0,
        ro: float = 0.0,
        tgrlc: float = 0.0,
        tgmult: float = 0.0,
        hc: float = 0.0,
        tc: float = 0.0,
    ) -> bool:
        """Create a MAT_THERMAL_ISOTROPIC keyword.

        Parameters
        ----------
        tmid : int
            Thermal material ID.
        ro : float
            Mass density.
        tgrlc : float
            Thermal generation rate load curve.
        tgmult : float
            Thermal generation rate multiplier.
        hc : float
            Specific heat.
        tc : float
            Thermal conductivity.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatThermalIsotropic()
        kw.tmid = tmid
        kw.ro = ro
        kw.tgrlc = tgrlc
        kw.tgmult = tgmult
        kw.hc = hc
        kw.tc = tc

        self._deck.append(kw)
        logger.debug(f"Created MAT_THERMAL_ISOTROPIC with tmid={tmid}")
        return True

    def create_mat_rigid(
        self,
        mid: int,
        ro: float,
        e: float,
        pr: float,
    ) -> bool:
        """Create a MAT_RIGID keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float
            Mass density.
        e : float
            Young's modulus.
        pr : float
            Poisson's ratio.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatRigid()
        kw.mid = mid
        kw.ro = ro
        kw.e = e
        kw.pr = pr

        self._deck.append(kw)
        logger.debug(f"Created MAT_RIGID with mid={mid}")
        return True

    def create_mat_piecewise_linear_plasticity(
        self,
        mid: int,
        ro: float,
        e: float,
        pr: float = 0.3,
        sigy: float = 0.0,
        etan: float = 0.0,
    ) -> bool:
        """Create a MAT_PIECEWISE_LINEAR_PLASTICITY keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float
            Mass density.
        e : float
            Young's modulus.
        pr : float
            Poisson's ratio.
        sigy : float
            Yield stress.
        etan : float
            Tangent modulus.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatPiecewiseLinearPlasticity()
        kw.mid = mid
        kw.ro = ro
        kw.e = e
        kw.pr = pr
        kw.sigy = sigy
        kw.etan = etan

        self._deck.append(kw)
        logger.debug(f"Created MAT_PIECEWISE_LINEAR_PLASTICITY with mid={mid}")
        return True

    def create_mat_rigid_discrete(
        self,
        mid: int,
        ro: float = 0.0,
        e: float = 0.0,
        pr: float = 0.0,
    ) -> bool:
        """Create a MAT_RIGID_DISCRETE keyword.

        Parameters
        ----------
        mid : int
            Material identification.
        ro : float, optional
            Mass density. Default is 0.0.
        e : float, optional
            Young's modulus. Default is 0.0.
        pr : float, optional
            Poisson's ratio. Default is 0.0.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.MatRigidDiscrete()
        kw.mid = mid
        kw.ro = ro
        kw.e = e
        kw.pr = pr

        self._deck.append(kw)
        logger.debug(f"Created MAT_RIGID_DISCRETE with mid={mid}")
        return True
