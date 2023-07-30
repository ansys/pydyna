"""
DEM API
=======

Module for creating a DEM DYNA input deck.
"""

import logging

from .dynabase import *  # noqa : F403


class DynaDEM(DynaBase):
    """Contains methods for creating a keyword related to a discrete element method."""

    def __init__(self):
        DynaBase.__init__(self)

    def set_des(self, ndamp=0.0, tdamp=0.0, frics=0.0, fricr=0.0, normk=0.01, sheark=0.2857):
        """Set global control parameters for discrete element spheres.

        Parameters
        ----------
        ndamp : float, optional
            Normal damping coefficient. The default is ``0.0``.
        tdamp : float, optional
            Tangential damping coefficient. The default is ``0.0``.
        frics : float, optional
            Static coefficient of friction. The default is ``0.0``.

            - EQ.0: 3 DOF
            - NE.0: 6 DOF

        fricr : float, optional
            Rolling friction coefficient. The default is ``0.0``.
        normk : float, optional
            Scale factor of the normal spring constant.  The default is ``0.01``.
        sheark : float, optional
            Ratio between the sheark/normk. The default is ``0.2857``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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

    def create_define_de_mesh_surface(self, sid, type, despid, desxid, nquad=1, nsid=0, rsf=1.0):
        """Create discrete element sphere (DES) elements on the surface of shell elements.

        Parameters
        ----------
        sid : int
            Part or part set ID for the region of the mesh to place DES elements on.
        type : int
            SID type:

            - type=0: Part set ID
            - type=1: Part ID

        despid : int
            Part ID for the generated DES elements.
        desxid : int
            Section ID for the generated DES elements.
        nquad : int, optional
            Number of equally spaced DES elements created on a shell element in each
            local shell direction. The default is ``1``.
        nsid : int, optional
            If defined, this card creates a node set with ID NSID for the nodes generated
            by this card. The default is ``0``.
        rsf : float, optional
            Scale factor for determining the DES radius. The default is ``1.0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateDefineDEMeshSurface(
            DefineDEMeshSurfaceRequest(
                sid=sid,
                type=type,
                despid=despid,
                desxid=desxid,
                nquad=nquad,
                nsid=nsid,
                rsf=rsf,
            )
        )
        logging.info("Define discrete element mesh surface Created...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        DynaBase.save_file(self)


class DEMAnalysis:
    """Activates DEM analysis and defines associated control parameters."""

    def __init__(self):
        self.defined_des = False
        self.stub = DynaBase.get_stub()

    def set_des(
        self,
        normal_damping_coeff=0.0,
        tangential_damping_coeff=0.0,
        static_friction_coeff=0.0,
        rolling_friction_coeff=0.0,
        normal_spring_constant_sf=0.01,
        ratio=0.2857,
    ):
        """Set global control parameters for discrete element spheres.

        Parameters
        ----------
        normal_damping_coeff : float, optional
            Normal damping coefficient. The default value is ``0.0``.
        tangential_damping_coeff : float, optional
            Tangential damping coefficient. The default value is ``0.0``.
        static_friction_coeff : float, optional
            Static coefficient of friction. The default is ``0.0``.
        rolling_friction_coeff : float, optional
           Rolling coefficient of friction. The default is ``0.0``.
        normal_spring_constant_sf : float, optional
           Normal spring constant. The default is ``0.01``.
        ratio : float, optional
           Ratio. The default is ``0.2857``.
        """
        self.defined_des = True
        self.ndamp = normal_damping_coeff
        self.tdamp = tangential_damping_coeff
        self.frics = static_friction_coeff
        self.fricr = rolling_friction_coeff
        self.normk = normal_spring_constant_sf
        self.sheark = ratio

    def create(self):
        """Create a DEM analysis."""
        if self.defined_des:
            self.stub.CreateControlDiscreteElement(
                ControlDiscreteElementRequest(
                    ndamp=self.ndamp,
                    tdamp=self.tdamp,
                    frics=self.frics,
                    fricr=self.fricr,
                    normk=self.normk,
                    sheark=self.sheark,
                )
            )
