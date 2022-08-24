"""Module to create IGA dyna input deck"""

import logging

from .dynabase import *


class DynaIGA(DynaBase):
    """Contains methods to create keyword related to IGA"""

    def __init__(self, hostname = 'localhost'):
        DynaBase.__init__(self, hostname)
        self.casetype = CaseType.IGA

    def create_section_igashell(self, secid, elform, shrf, thickness):
        """Create *SECTION_IGA_SHELL keyword
        Parameters
        ----------
        secid : int
            Section ID. SECID is referenced on the *PART card. A unique number or label must be specified.
        elform : int
            Element formulation.
        shrf : float
            Shear correction factor which scales the transverse shear stress.
        thickness : float
            Shell thickness.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSectionIGAShell(
            SectionIGAShellRequest(
                secid=secid, elform=elform, shrf=shrf, thickness=thickness
            )
        )
        logging.info("Section IGAShell 1 Created...")
        return ret
