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

    def set_des(self, num_timestep=1, boxid=0, space_dimension=3, neighbors=150, approximation_theory=0, max_velocty=1e15):
        """Provide controls related to SPH.

        Parameters
        ----------
        num_timestep : int
            Number of time steps between particle sorting.
        boxid : int
            SPH approximations are computed inside a specified box.When a particle has gone outside the BOX, it is deactivated.
        space_dimension : int
            Space dimension for SPH particles,EQ.3: 3D problems EQ.2: 2D plane strain problems EQ.-2: 2D axisymmetric problems
        neighbors : int
            Defines the initial number of neighbors per particle.
        approximation_theory : int
            Particle approximation theory.
        max_velocty : float
            Maximum value for velocity for the SPH particles.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateControlSPH(
            CreateControlSPHRequest(
                ncbs=num_timestep,
                boxid=boxid,
                idim=space_dimension,
                nmneigh=neighbors,
                form=approximation_theory,
                maxv=max_velocty,
            )
        )
        logging.info("Control SPH Created...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        DynaBase.save_file(self)