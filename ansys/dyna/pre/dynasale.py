"""Module to create Structural ALE dyna input deck"""

import logging

from .dynabase import *
from enum import Enum

class AdvectionMethod(Enum):
    DONOR_CELL_WITH_HALF_INDEX_SHIFT = 1
    VAN_LEER_WITH_HIS = 2
    DONOR_CELL_WITH_HIS = 3
    FINITE_VOLUME_METHOD = 6

class FillDirection(Enum):
    INSIDE_THE_GEOMETRY = 0
    OUTSIDE_THE_GEOMETRY = 1


class ControlPoint:
    def __init__(self,number,position,ratio):
        self.number = number
        self.position = position
        self.ratio = ratio

class StructuredMesh:
    num_meshpart=0

    def __init__(self,stub,meshid,partid):
        self.meshid = meshid
        self.partid = partid
        self.stub = stub
        StructuredMesh.num_meshpart += 1

    def fill(self, material,geometry_type="NULL", nsample=4,define_geometry_parameters=[0,0,0,0,0],inout=FillDirection.INSIDE_THE_GEOMETRY,vid=0,reference_pressure = 0):
        """Perform volume filling operations on a structured ALE mesh.

        Parameters
        ----------
        material_name : string
            material name.
        nsample : int
            Number of sampling points.
        geometry_type : string
            Geometry types. They are: PARTSET, PART, SEGSET, PLANE, CYLINDER, BOXCOR, BOXCPT and SPHERE.
        define_geometry_parameters : list
            These values have different definitions for different options. 
        in_out : int
            To fill inside or outside of the geometry.
        vid : int
            This flag is used to assign initial velocity to material filling the domain.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        material.create(self.stub)
        ret = self.stub.ALECreateStructuredMultiMaterialGroup(
            ALECreateStructuredMultiMatGroupRequest(nmmgnm=material.name,mid=material.material_id,eosid=material.eos_id,pref=reference_pressure)
        )

        logging.info(f"Material {material.name} Created...")
        if geometry_type.upper() != "NULL":
            ret = self.stub.ALECreateStructuredMeshVolumeFilling(
                ALECreateStructuredMeshVolumeFillingRequest(mshid=self.meshid, ammgto=material.name, nsample=nsample,geom=geometry_type.upper(),vid=vid,inout=inout.value,e=define_geometry_parameters))
            logging.info(f"Material {material.name} filled in Mesh {self.meshid}...")
        return ret

    def refine(self, refine_factor_x=1, refine_factor_y=1, refine_factor_z=1):
        """refine existing structured ALE (S-ALE) meshes.

        Parameters
        ----------
        refine_factor_x/y/y : int
            Refinement factor for each local direction.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ALECreateStructuredMeshRefine(
            ALECreateStructuredMeshRefineRequest(mshid=self.meshid, ifx=refine_factor_x, ify=refine_factor_y, ifz=refine_factor_z)
            )
        logging.info(f"Mesh {self.meshid} Refined...")
        return ret

    def initial_detonation(self,detonation_point):
        """Create *ICFD_DATABASE_DRAG keyword

        Parameters
        ----------
        detonation_point : list [x,y,z]
            x,y,z-coordinate of detonation point.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        partid = self.partid
        ret = self.stub.CreateInitDetonation(InitDetonationRequest(pid=partid,coord=detonation_point,lt=0))
        logging.info("Location of high explosive detonation Defined...")
        return ret



class DynaSALE(DynaBase):
    """Setup SALE simulation process"""

    def __init__(self, hostname = 'localhost',filenames=[]):
        DynaBase.__init__(self, hostname)
        DynaBase.open_files(self,filenames=filenames) 
        self.stub.CreateDBSALE(DBSALERequest(switch=1))       
        
    def set_termination(self, endtime):
        self.stub.CreateTermination(TerminationRequest(endtim=endtime))

    def set_output_interval(self, database_plot_interval):
        self.stub.CreateDBBinary(DBBinaryRequest(filetype="D3PLOT",dt=database_plot_interval))

    def set_analysis_type(self, num_of_cycle=1, method=AdvectionMethod.DONOR_CELL_WITH_HALF_INDEX_SHIFT,background_pressure=0):
        """Setup analysis type.

        Parameters
        ----------
        num_of_cycle : float
            Total time of simulation for the fluid problem.
        method : float
            Time step for the fluid problem.
        background_pressure : int
            Reference pressure to compute the internal forces

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.ALECreateControl(ControlALERequest(dct=0,nadv=num_of_cycle, meth=method.value,afac=0,end=1e20,aafac=1,vfact=1e-6,pref=background_pressure))
        logging.info("Setup Analysis...")
        return ret

    def create_mesh(self, control_points_x,control_points_y,control_points_z):
        """Create mesh.

        Parameters
        ----------
        control_points_x/y/z : list [[N1,X1,ratio1],[N2,X2,ratio2],...]
            Defines a one-dimensional mesh using control points. Each control point consists of a node number (N) and a coordinate (X).
            ratio : Ratio for progressive mesh spacing.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        nx=[]
        xx=[]
        ratiox=[]
        for i in range(len(control_points_x)):
            nx.append(control_points_x[i].number)
            xx.append(control_points_x[i].position)
            ratiox.append(control_points_x[i].ratio)
        ny=[]
        xy=[]
        ratioy=[]
        for i in range(len(control_points_y)):
            ny.append(control_points_y[i].number)
            xy.append(control_points_y[i].position)
            ratioy.append(control_points_y[i].ratio)
        nz=[]
        xz=[]
        ratioz=[]
        for i in range(len(control_points_z)):
            nz.append(control_points_z[i].number)
            xz.append(control_points_z[i].position)
            ratioz.append(control_points_z[i].ratio)

        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2,sfo=1,n=nx,x=xx,ratio=ratiox)
            )
        cpidx = ret.cpid
        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2,sfo=1,n=ny,x=xy,ratio=ratioy)
            )
        cpidy = ret.cpid
        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2,sfo=1,n=nz,x=xz,ratio=ratioz)
            )
        cpidz = ret.cpid
        ret = self.stub.ALECreateStructuredMesh(
            ALECreateStructuredMeshRequest(nbid=2000001,ebid=2000001,cpidx=cpidx,cpidy=cpidy,cpidz=cpidz)
        )
        meshid = ret.meshid
        partid = ret.partid
        mesh = StructuredMesh(stub=self.stub,meshid=meshid,partid=partid)

        logging.info(f"ALE Structured mesh {meshid} Created...")
        return mesh

    def set_output_database(self, matsum=0,glstat=0):
        """obtain output files containing results information.

        Parameters
        ----------
        matsum : float
            Time interval between outputs of part energies.
        glstat : float      
            Time interval between outputs of global statistics and energies.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        if matsum>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="MATSUM", dt=matsum, binary=1, lcur=0,ioopt=0)
            )
        if glstat>0:
            self.stub.CreateDBAscii(
                DBAsciiRequest(type="GLSTAT", dt=glstat, binary=1, lcur=0,ioopt=0)
            )
        ret = 1
        logging.info("Output Setting...")
        return ret
