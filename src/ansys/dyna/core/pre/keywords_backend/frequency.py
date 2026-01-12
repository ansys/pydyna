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

"""Frequency domain keyword creation methods for the keywords backend."""

import logging

logger = logging.getLogger(__name__)


class FrequencyKeywordsMixin:
    """Mixin class providing frequency domain keyword creation methods."""

    def create_frequency_domain_frf(
        self,
        n1: int = 0,
        n1typ: int = 0,
        dof1: int = 1,
        vad1: int = 1,
        fnmax: float = 0.0,
        dampf: float = 0.0,
        n2: int = 0,
        n2typ: int = 1,
        dof2: int = 1,
        vad2: int = 1,
        fmin: float = 0.0,
        fmax: float = 0.0,
        nfreq: int = 100,
    ) -> bool:
        """Create a FREQUENCY_DOMAIN_FRF keyword.

        Parameters
        ----------
        n1 : int, optional
            Input node/set ID. Default is 0.
        n1typ : int, optional
            Input node type. Default is 0.
        dof1 : int, optional
            Input DOF. Default is 1.
        vad1 : int, optional
            Input VAD flag. Default is 1.
        fnmax : float, optional
            Maximum natural frequency. Default is 0.0.
        dampf : float, optional
            Modal damping factor. Default is 0.0.
        n2 : int, optional
            Output node/set ID. Default is 0.
        n2typ : int, optional
            Output node type. Default is 1.
        dof2 : int, optional
            Output DOF. Default is 1.
        vad2 : int, optional
            Output VAD flag. Default is 1.
        fmin : float, optional
            Minimum frequency. Default is 0.0.
        fmax : float, optional
            Maximum frequency. Default is 0.0.
        nfreq : int, optional
            Number of frequencies. Default is 100.

        Returns
        -------
        bool
            True if successful.
        """
        from ansys.dyna.core.keywords import keywords

        kw = keywords.FrequencyDomainFrf()
        kw.n1 = n1
        kw.n1typ = n1typ
        kw.dof1 = dof1
        kw.vad1 = vad1
        kw.fnmax = fnmax
        kw.dampf = dampf
        kw.n2 = n2
        kw.n2typ = n2typ
        kw.dof2 = dof2
        kw.vad2 = vad2
        kw.fmin = fmin
        kw.fmax = fmax
        kw.nfreq = nfreq

        self._deck.append(kw)
        logger.debug(f"Created FREQUENCY_DOMAIN_FRF: fmin={fmin}, fmax={fmax}, nfreq={nfreq}")
        return True
