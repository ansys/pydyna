"""Module to create ICFD dyna input deck"""

import logging

from .dynabase import *

class DynaICFD(DynaBase):
    """Contains methods to create keyword related to ICFD"""

    def __init__(self):
        DynaBase.__init__(self)

    def create_control_time(self, tim, dt):
        """Create *ICFD_CONTROL_TIME keyword
        Parameters
        ----------
        tim : float
            Total time of simulation for the fluid problem.
        dt : float
            Time step for the fluid problem.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateControlTime(ICFDControlTimeRequest(tim=tim, dt=dt))
        logging.info("ICFD control time Created...")
        return ret

    def create_section_icfd(self, sid):
        """Create *ICFD_SECTION keyword
        Parameters
        ----------
        sid : int
            Section identifier.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateSection(ICFDSectionRequest(sid=sid))
        logging.info("ICFD Section Created...")
        return ret

    def create_mat_icfd(self, mid,flg,ro,vis):
        """Create *ICFD_MAT keyword
        Parameters
        ----------
        mid : int
            Material ID.
        flg : int
            Flag to choose between fully incompressible, slightly compressible, or barotropic flows: 
            EQ.0: Vacuum (free surface problems only) 
            EQ.1: Fully incompressible fluid.
        ro : float
            Flow density.
        vis : float
            Dynamic viscosity.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateMat(ICFDMatRequest(mid=mid,flg=flg,ro=ro,vis=vis))
        logging.info("ICFD material Created...")
        return ret

    def create_part_icfd(self, pid,secid,mid):
        """Create *ICFD_PART keyword
        Parameters
        ----------
        pid : int
            Part identifier for fluid surfaces.
        secid : int
            Section identifier defined with the *ICFD_SECTION card.
        mid : int
            Material identifier defined with the *ICFD_MAT card.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreatePart(ICFDPartRequest(pid=pid,secid=secid,mid=mid))
        logging.info("ICFD part Created...")
        return ret

    def create_part_vol(self, pid,secid,mid,spids):
        """Create *ICFD_PART_VOL keyword
        Parameters
        ----------
        pid : int
            Part identifier for fluid volumes.
        secid : int
            Section identifier defined by the *ICFD_SECTION card.
        mid : int
            Material identifier.
        spids : list
            List of Part IDs for the surface elements that define the volume mesh.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreatePartVol(ICFDPartVolRequest(pid=pid,secid=secid,mid=mid,spids=spids))
        logging.info("ICFD part volume Created...")
        return ret    

    def create_db_drag(self, pid):
        """Create *ICFD_DATABASE_DRAG keyword
        Parameters
        ----------
        pid : int
            Part ID of the surface where the drag force will be computed.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateDBDrag(ICFDDBDragRequest(pid=pid))
        logging.info("ICFD database drag Created...")
        return ret  

    def create_bdy_prescribed_vel(self, pid,dof,vad,lcid):
        """Create *ICFD_BOUNDARY_PRESCRIBED_VEL keyword
        Parameters
        ----------
        pid : int
            PID for a fluid surface.
        dof : int
            Applicable degrees of freedom:
            EQ.1: x-degree of freedom.
            EQ.2: y-degree of freedom. 
            EQ.3: z-degree of freedom. 
            EQ.4: Normal direction degree of freedom.
        vad : int
            Velocity flag:
            EQ.1: Linear velocity. 
            EQ.2: Angular velocity. 
            EQ.3: Parabolic velocity profile. 
            EQ.4: Activates synthetic turbulent field on part.
        lcid : int
            Load curve ID used to describe motion value versus time.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateBdyPrescribedVel(ICFDBdyPrescribedVelRequest(pid=pid,dof=dof,vad=vad,lcid=lcid))
        logging.info("ICFD boundary prescribed velocity Created...")
        return ret   

    def create_bdy_prescribed_pre(self, pid,lcid):
        """Create *ICFD_BOUNDARY_PRESCRIBED_PRE keyword
        Parameters
        ----------
        pid : int
            PID for a fluid surface..
        lcid : int
            Load curve ID to describe the pressure value versus time.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateBdyPrescribedPre(ICFDBdyPrescribedPreRequest(pid=pid,lcid=lcid))
        logging.info("ICFD boundary prescribed pressure Created...")
        return ret 

    def create_bdy_free_slip(self, pid):
        """Create *ICFD_BOUNDARY_FREESLIP keyword
        Parameters
        ----------
        pid : int
            PID of the fluid surface where a free-slip boundary condition is applied.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateBdyFreeSlip(ICFDBdyFreeSlipRequest(pid=pid))
        logging.info("ICFD boundary freeslip Created...")
        return ret 

    def create_bdy_non_slip(self, pid):
        """Create *ICFD_BOUNDARY_NONSLIP keyword
        Parameters
        ----------
        pid : int
            PID of the fluid surface where a non-slip boundary condition is applied.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ICFDCreateBdyNonSlip(ICFDBdyNonSlipRequest(pid=pid))
        logging.info("ICFD boundary nonslip Created...")
        return ret

    def mesh_create_volume(self, volid,pids):
        """Create *MESH_VOLUME keyword
        Parameters
        ----------
        volid : int
            ID assigned to the new volume.
        pids : list
            list of Part IDs for the surface elements that are used to define the volume.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.MESHCreateVolume(MeshVolumeRequest(volid=volid,pids=pids))
        logging.info("MESH volume Created...")
        return ret

    def mesh_create_bl(self, pid,nelth):
        """Create *MESH_BL keyword
        Parameters
        ----------
        pid : int
            Part identifier for the surface element.
        nelth : int
            Number of elements normal to the surface (in the boundary layer) is NELTH+1.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.MESHCreateBl(MeshBlRequest(pid=pid,nelth=nelth))
        logging.info("MESH boundary-layer Created...")
        return ret   