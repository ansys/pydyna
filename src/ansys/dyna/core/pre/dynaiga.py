"""
IGA API
==========

Module for creating an IGA DYNA input deck.
"""

import logging

from .dynabase import *  # noqa : F403


class DynaIGA(DynaBase):
    """Contains methods for creating a keyword related to IGA."""

    def __init__(self):
        DynaBase.__init__(self)
        self.casetype = CaseType.IGA

    def create_section_igashell(self, secid, elform, shrf, thickness):
        """Define section properties for isogeometric shell elements.

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
