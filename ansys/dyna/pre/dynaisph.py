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
        self.isphanalysis = ISPHAnalysis()

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
        self.isphanalysis.create()
        DynaBase.save_file(self)

class ISPHAnalysis:
    """Provide controls related to SPH (Smooth Particle Hydrodynamics)."""

    def __init__(self, num_timestep=1):
        self.stub = DynaBase.get_stub()
        self.num_timestep = num_timestep 
        self.box = None
        self.space_dimension = 3
        self.neighbors = 150
        self.approximation_theory = 13
        self.particle_deactivation = 1e15
        self.velocity_scaling = 0
        
    def set_num_timestep(self, num_timestep):
        """Set number of time steps between particle sorting."""
        self.num_timestep = num_timestep

    def set_box(self, box):
        """Define box,SPH approximations are computed inside a specified box.
        
        Parameters
        ----------
        box : Box
            When a particle has gone outside the BOX, it is deactivated. This will save computational time by eliminating particles that no longer interact with the structure.
        """
        self.box = box

    def set_neighbors(self, neighbors):
        """Define the initial number of neighbors per particle."""
        self.neighbors = neighbors

    def set_particle_deactivation(self,deactivation) :
        """Define the type of BEM matrices as well as the way they are assembled."""
        self.particle_deactivation = deactivation

    def set_velocity_scaling(self,scaling) :
        """Define the type of BEM matrices as well as the way they are assembled."""
        self.velocity_scaling = scaling    

    def create(self):
        """Create ISPHAnalysis."""
        if self.box==None:
            boxid = 0
        else:
            boxid = self.box.create(self.stub)
        if self.velocity_scaling != 0:
            maxv = -self.velocity_scaling
        else:
            maxv = self.particle_deactivation
        ret = self.stub.CreateControlSPH(
            ControlSPHRequest(
                ncbs=self.num_timestep,
                boxid=boxid,
                idim=self.space_dimension,
                nmneigh=self.neighbors,
                form=self.approximation_theory,
                maxv=maxv
            )
        )
        logging.info("Control SPH Created...")

class SPHSection:
    """Define section properties for SPH particles."""

    def __init__(
        self,
        cslh=1.2,
        hmin=0.2,
        hmax=2.0,
        sphini=0
    ):
        stub = DynaBase.get_stub()
        ret = stub.CreateSectionSPH(
            SectionSPHRequest(
                cslh=cslh,
                hmin=hmin,
                hmax=hmax,
                sphini=sphini
            )
        )
        self.id = ret.id

class MassflowPlane:
    """Measure SPH mass flow rate across a defined plane.

    Parameters
    ----------
    particles : NodeSet,PartSet
        Node set or part set specifying the SPH particles to be measured.
    surface : PartSet
        Part set ID or part ID defining the surface across which the flow rate is measured.
    """

    def __init__(self,particles,surface):
        self.particles = particles
        self.surface = surface

    def create(self,stub):
        """Create mass flow plane."""
        self.particles.create(stub)
        self.surface.create(stub)
        pid = self.particles.id
        sid = self.surface.id
        if self.particles.type == "NODESET":
            ptype = 0
        elif self.particles.type == "NODE":
            ptype = 1
        elif self.particles.type == "PARTSET":
            ptype = 2
        elif self.particles.type == "PART":
            ptype = 3
            pid = self.particles.get_pid()
        else:
            print('invalid set type.')
        if self.surface.type == "PARTSET":
            stype = 0
        elif self.surface.type == "PART":
            stype = 1
            sid = self.surface.get_pid()
        stub.CreateDefineSPHMassflowPlane(DefineSPHMassflowPlaneRequest(prtclsid=pid, surfsid=sid,ptype=ptype,stype=stype))

class ISPHFluidPart(Part):
    """Generate SPH particles inside a given box.

    Parameters
    ----------
    minpoint : Point
        Minimum x,y,z-coordinate.
    length : Point
        Box length in the x,y,z-direction.
    numdirx : int
        Number of SPH particles in the x-direction.
    numdiry : int
        Number of SPH particles in the y-direction.
    numdirz : int
        Number of SPH particles in the z-direction.
    """

    def __init__(self,pid,minpoint,length,numdirx,numdiry,numdirz):
        Part.__init__(self, pid)
        self.stub = DynaBase.get_stub()
        self.type = "ISPHFLUID"
        self.minpoint = minpoint
        self.length = length
        self.numdirx = numdirx
        self.numdiry = numdiry
        self.numdirz = numdirz
        #section
        self.cslh = 1.2
        self.hmin=0.2
        self.hmax=2.0
        self.sphini=0
        #massflow plane
        self.massflowplane = None

    def set_smoothing_length(self,initial,min,max,optional):
        """Calculate the smoothing length of the particles.

        Parameters
        ----------
        initial : float
            Constant used to calculate the initial smoothing length of the particles.
        min : float
            Scale factor for the minimum smoothing length.
        max : float
            Scale factor for the maximum smoothing length.
        optional : float
            Optional initial smoothing length (overrides true smoothing length).
        """
        self.cslh = initial
        self.hmin=min
        self.hmax=max
        self.sphini=optional

    def create_particles(self):
        """Create SPH particles inside a given box."""
        coords = (self.minpoint.x,self.minpoint.y,self.minpoint.z,self.length.x,self.length.y,self.length.z)
        numparticles = (self.numdirx,self.numdiry,self.numdirz)
        self.stub.CreateDefineSPHMeshBox(DefineSPHMeshBoxRequest(ipid=self.id, coords=coords,numparticles=numparticles))  
    
    def create_massflow_plane(self,surfaces):
        """Measure SPH mass flow rate across a defined plane."""
        self.massflowplane = MassflowPlane(PartSet([self.id]),surfaces)

    def set_property(self):
        """Set properties for SPH fluid part."""
        self.create_particles()
        self.massflowplane.create(self.stub)
        sec = SPHSection(cslh=self.cslh,hmin=self.hmin,hmax=self.hmax,sphini=self.sphini)
        self.secid = sec.id
        self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=self.id,
                secid=self.secid,
                mid=self.mid,
                eosid=self.eosid,
                hgid=self.hgid,
                grav=self.grav,
                adpopt=self.adpopt,
                tmid=self.tmid,
            )
        )

class ISPHStructPart(Part):
    """Generate and place SPH elements on the surface of triangular shell elements.

    Parameters
    ----------
    couple_partset : PartSet
        Part or part set ID for the region of the mesh upon which the SPH elements will be placed.
    space : float
        Maximum space between SPH elements.
    """

    def __init__(self,pid,couple_partset,space):
        Part.__init__(self, pid)
        self.stub = DynaBase.get_stub()
        self.type = "ISPHSTRUCT"
        self.couple_partset = couple_partset
        self.space = space
        #section
        self.cslh = 1.2
        self.hmin=0.2
        self.hmax=2.0
        self.sphini=0

    def set_smoothing_length(self,initial,min,max,optional):
        """Calculate the smoothing length of the particles.

        Parameters
        ----------
        initial : float
            Constant used to calculate the initial smoothing length of the particles.
        min : float
            Scale factor for the minimum smoothing length.
        max : float
            Scale factor for the maximum smoothing length.
        optional : float
            Optional initial smoothing length (overrides true smoothing length).
        """
        self.cslh = initial
        self.hmin=min
        self.hmax=max
        self.sphini=optional

    def create_particles(self):
        """Create SPH elements on the surface of triangular shell elements."""
        sid = self.couple_partset.create(self.stub)
        if self.couple_partset.type == "PARTSET":
            type = 0
        else:
            type = 1
            sid = self.couple_partset.get_pid()
        self.stub.CreateDefineSPHMeshSurface(DefineSPHMeshSurfaceRequest(sid=sid, type=type,sphpid=self.id,space=self.space))  

    def set_property(self):
        """Set properties for SPH structural part."""
        self.create_particles()
        sec = SPHSection(cslh=self.cslh,hmin=self.hmin,hmax=self.hmax,sphini=self.sphini)
        self.secid = sec.id
        self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=self.id,
                secid=self.secid,
                mid=self.mid,
                eosid=self.eosid,
                hgid=self.hgid,
                grav=self.grav,
                adpopt=self.adpopt,
                tmid=self.tmid,
            )
        )