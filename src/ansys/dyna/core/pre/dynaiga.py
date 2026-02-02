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

"""Module for creating an IGA DYNA input deck."""

import logging

from .dynabase import *  # noqa : F403


class DynaIGA(DynaBase):
    """Contain methods for creating a keyword related to IGA."""

    def __init__(self):
        DynaBase.__init__(self)
        self.casetype = CaseType.IGA

    def create_section_igashell(self, secid, elform, shrf, thickness):
        r"""Define section properties for isogeometric shell elements.

        Parameters
        ----------
        secid : int
            Section ID. ``SECID`` is referenced on the ``\*PART`` card.
            A unique number or label must be specified.
        elform : int
            Element formulation.
        shrf : float
            Shear correction factor, which scales the transverse shear stress.
        thickness : float
            Shell thickness.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateSectionIGAShell(
            SectionIGAShellRequest(secid=secid, elform=elform, shrf=shrf, thickness=thickness)
        )
        logging.info("Section IGAShell 1 Created...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        DynaBase.save_file(self)
        self.create_control_contact(rwpnal=1.0, ignore=1, igactc=1)
