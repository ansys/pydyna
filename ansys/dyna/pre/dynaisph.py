"""
ISPH API
==========

Module to create ISPH dyna input deck
"""

import logging

from .dynabase import *  # noqa : F403


class DynaISPH(DynaBase):
    """Contains methods to create keyword related to incompressible smooth particle hydrodynamics."""

    def __init__(self):
        DynaBase.__init__(self)

    def set_des(self, ndamp=0.0, tdamp=0.0, frics=0.0, fricr=0.0, normk=0.01, sheark=0.2857):
        """Define global control parameters for discrete element spheres.

        Parameters
        ----------
        ndamp : float
            Normal damping coefficient.
        tdamp : float
            Tangential damping coefficient.
        frics : float
            Static coefficient of friction,EQ.0: 3 DOF,NE.0: 6 DOF.
        fricr : float
            Rolling friction coefficient.
        normk : float
             Scale factor of normal spring constant.
        sheark : float
             ratio between sheark/normk.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlDiscreteElement(
            ControlDiscreteElementRequest(
                ndamp=ndamp,
                tdamp=tdamp,
                frics=frics,
                fricr=fricr,
                normk=normk,
                sheark=sheark,
            )
        )
        logging.info("Control DES Created...")
        return ret

    

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        DynaBase.save_file(self)