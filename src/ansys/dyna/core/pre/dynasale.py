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

"""
Airbag API
==========

Module for creating a S-ALE (Structured ALE) DYNA input deck.
"""

from enum import Enum
import logging

from .dynabase import *  # noqa : F403


class AdvectionMethod(Enum):
    DONOR_CELL_WITH_HALF_INDEX_SHIFT = 1
    VAN_LEER_WITH_HIS = 2
    DONOR_CELL_WITH_HIS = 3
    FINITE_VOLUME_METHOD = 6


class FillDirection(Enum):
    INSIDE_THE_GEOMETRY = 0
    OUTSIDE_THE_GEOMETRY = 1


class ControlPoint:
    """Provides spacing information for generating a 3D S-ALE mesh.

    Parameters
    ----------
    number : int
        Control point node number.
    position : float
        Control point position.
    ratio : float
        Ratio for progressive mesh spacing.
    """

    def __init__(self, number, position, ratio):
        self.number = number
        self.position = position
        self.ratio = ratio


class StructuredMesh(BaseObj):
    """Generates a structured 2D or 3D mesh and invokes the S-ALE solver."""

    def __init__(self, control_points_x, control_points_y, control_points_z):
        self.stub = DynaBase.get_stub()
        self.control_points_x = control_points_x
        self.control_points_y = control_points_y
        self.control_points_z = control_points_z
        self.refine_factor_x = 1
        self.refine_factor_y = 1
        self.refine_factor_z = 1
        self.fillings = []
        self.type = "structured_mesh"

    def fill(
        self,
        material,
        geometry_type="NULL",
        nsample=4,
        define_geometry_parameters=[0, 0, 0, 0, 0],
        inout=FillDirection.INSIDE_THE_GEOMETRY,
        vid=0,
        reference_pressure=0,
    ):
        """Perform volume-filling operations on a S-ALE mesh.

        Parameters
        ----------
        material_name : string
            Material name.
        geometry_type : string
            Geometry type. The default is ``"Null"``. Options are:

            - BOXCOR
            - BOXCPT
            - CYLINDER
            - PARTSET
            - PART
            - PLANE
            - SEGSET
            - SPHERE

        nsample : int, optional
            Number of sampling points. The default is ``4``.
        define_geometry_parameters : list
            List of values having different definitions for different options.
            The default is ``[0, 0, 0, 0, 0]``.
        in_out : int, optional
            Flag for whether to fill inside or outside of the geometry. The
            default is ``INSIDE_THE_GEOMETRY``.
        vid : int, optional
            Flag for assigning the initial velocity to the material filling the domain.
            The default is ``0``.
        reference_pressure :

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.fillings.append(
            [
                material,
                geometry_type,
                nsample,
                define_geometry_parameters,
                inout,
                vid,
                reference_pressure,
            ]
        )

    def refine(self, refine_factor_x=1, refine_factor_y=1, refine_factor_z=1):
        """Refine existing S-ALE meshes.

        Parameters
        ----------
        refine_factor_x : int, optional
            Refinement factor for the x-direction. The default is ``1``.
        refine_factor_y : int, optional
            Refinement factor for the y-direction. The default is ``1``.
        refine_factor_z : int, optional
            Refinement factor for the z-direction. The default is ``1``.
        """
        self.refine_factor_x = refine_factor_x
        self.refine_factor_y = refine_factor_y
        self.refine_factor_z = refine_factor_z

    def initial_detonation(self, detonation_point):
        """Define a point for initiating the location of a high-explosive detonation.

        Parameters
        ----------
        detonation_point : Point
            Coordinates (x,y,z) of the detonation point.
        """
        self.detonation_point = detonation_point

    def create(self):
        """Create a mesh."""
        nx = []
        xx = []
        ratiox = []
        for i in range(len(self.control_points_x)):
            nx.append(self.control_points_x[i].number)
            xx.append(self.control_points_x[i].position)
            ratiox.append(self.control_points_x[i].ratio)
        ny = []
        xy = []
        ratioy = []
        for i in range(len(self.control_points_y)):
            ny.append(self.control_points_y[i].number)
            xy.append(self.control_points_y[i].position)
            ratioy.append(self.control_points_y[i].ratio)
        nz = []
        xz = []
        ratioz = []
        for i in range(len(self.control_points_z)):
            nz.append(self.control_points_z[i].number)
            xz.append(self.control_points_z[i].position)
            ratioz.append(self.control_points_z[i].ratio)

        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2, sfo=1, n=nx, x=xx, ratio=ratiox)
        )
        cpidx = ret.cpid
        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2, sfo=1, n=ny, x=xy, ratio=ratioy)
        )
        cpidy = ret.cpid
        ret = self.stub.ALECreateStructuredMeshCtrlPoints(
            ALECreateStructuredMeshControlPointsRequest(icase=2, sfo=1, n=nz, x=xz, ratio=ratioz)
        )
        cpidz = ret.cpid
        ret = self.stub.ALECreateStructuredMesh(
            ALECreateStructuredMeshRequest(nbid=2000001, ebid=2000001, cpidx=cpidx, cpidy=cpidy, cpidz=cpidz)
        )
        meshid = ret.meshid
        partid = ret.partid
        logging.info(f"ALE Structured mesh {meshid} Created...")

        for obj in self.fillings:
            material = obj[0]
            geometry_type = obj[1]
            nsample = obj[2]
            define_geometry_parameters = obj[3]
            inout = obj[4]
            vid = obj[5]
            reference_pressure = obj[6]
            material.create(self.stub)
            self.stub.ALECreateStructuredMultiMaterialGroup(
                ALECreateStructuredMultiMatGroupRequest(
                    nmmgnm=material.name,
                    mid=material.material_id,
                    eosid=material.eos_id,
                    pref=reference_pressure,
                )
            )
            logging.info(f"Material {material.name} Created...")
            if geometry_type.upper() != "NULL":
                self.stub.ALECreateStructuredMeshVolumeFilling(
                    ALECreateStructuredMeshVolumeFillingRequest(
                        mshid=meshid,
                        ammgto=material.name,
                        nsample=nsample,
                        geom=geometry_type.upper(),
                        vid=vid,
                        inout=inout.value,
                        e=define_geometry_parameters,
                    )
                )
                logging.info(f"Material {material.name} filled in Mesh {meshid}...")
        self.stub.ALECreateStructuredMeshRefine(
            ALECreateStructuredMeshRefineRequest(
                mshid=meshid,
                ifx=self.refine_factor_x,
                ify=self.refine_factor_y,
                ifz=self.refine_factor_z,
            )
        )
        logging.info(f"Mesh {meshid} Refined...")
        dpoint = [self.detonation_point.x, self.detonation_point.y, self.detonation_point.z]
        self.stub.CreateInitDetonation(InitDetonationRequest(pid=partid, coord=dpoint, lt=0))
        logging.info("Location of high explosive detonation Defined...")


class DynaSALE(DynaBase):
    """Sets up the S-ALE simulation process."""

    def __init__(self):
        DynaBase.__init__(self)
        self.stub.CreateDBSALE(DBSALERequest(switch=1))

    def create_mat_vacuum(self, mid, rho=1.0e-9):
        """Create a MAT_VACUUM keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        rho : float, optional
            Density. The default is ``1.0e-9``.

        Returns
        -------
        int
            The material ID.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_mat_vacuum(mid=mid, rho=rho)
        else:
            logging.warning("create_mat_vacuum is only available with the keywords backend")
            return None

    def create_mat_null(self, mid, ro=0.0, pc=0.0, mu=0.0):
        """Create a MAT_NULL keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float, optional
            Density. The default is ``0.0``.
        pc : float, optional
            Pressure cutoff. The default is ``0.0``.
        mu : float, optional
            Dynamic viscosity. The default is ``0.0``.

        Returns
        -------
        int
            The material ID.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_mat_null(mid=mid, ro=ro, pc=pc, mu=mu)
        else:
            logging.warning("create_mat_null is only available with the keywords backend")
            return None

    def create_mat_high_explosive_burn(self, mid, ro=0.0, d=0.0, pcj=0.0):
        """Create a MAT_HIGH_EXPLOSIVE_BURN keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float, optional
            Density. The default is ``0.0``.
        d : float, optional
            Detonation velocity. The default is ``0.0``.
        pcj : float, optional
            Chapman-Jouget pressure. The default is ``0.0``.

        Returns
        -------
        int
            The material ID.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_mat_high_explosive_burn(mid=mid, ro=ro, d=d, pcj=pcj)
        else:
            logging.warning("create_mat_high_explosive_burn is only available with the keywords backend")
            return None

    def create_mat_johnson_cook(
        self,
        mid,
        ro=0.0,
        g=0.0,
        e=0.0,
        pr=0.0,
        a=0.0,
        b=0.0,
        n=0.0,
        c=0.0,
        m=0.0,
        tm=0.0,
        tr=0.0,
        epso=0.0,
        cp=0.0,
        pc=0.0,
        spall=0.0,
        d1=0.0,
        d2=0.0,
        d3=0.0,
        d4=0.0,
        d5=0.0,
        c2_or_erod=0.0,
    ):
        """Create a MAT_JOHNSON_COOK keyword.

        Parameters
        ----------
        mid : int
            Material ID.
        ro : float, optional
            Density. The default is ``0.0``.
        g : float, optional
            Shear modulus. The default is ``0.0``.
        e : float, optional
            Young's modulus. The default is ``0.0``.
        pr : float, optional
            Poisson's ratio. The default is ``0.0``.
        a : float, optional
            Johnson-Cook parameter A. The default is ``0.0``.
        b : float, optional
            Johnson-Cook parameter B. The default is ``0.0``.
        n : float, optional
            Johnson-Cook parameter n. The default is ``0.0``.
        c : float, optional
            Johnson-Cook parameter C. The default is ``0.0``.
        m : float, optional
            Johnson-Cook parameter m. The default is ``0.0``.
        tm : float, optional
            Melt temperature. The default is ``0.0``.
        tr : float, optional
            Room temperature. The default is ``0.0``.
        epso : float, optional
            Reference strain rate. The default is ``0.0``.
        cp : float, optional
            Specific heat. The default is ``0.0``.
        pc : float, optional
            Pressure cutoff. The default is ``0.0``.
        spall : float, optional
            Spall type. The default is ``0.0``.
        d1-d5 : float, optional
            Damage parameters. The default is ``0.0``.
        c2_or_erod : float, optional
            C2 parameter or erosion flag. The default is ``0.0``.

        Returns
        -------
        int
            The material ID.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_mat_johnson_cook(
                mid=mid,
                ro=ro,
                g=g,
                e=e,
                pr=pr,
                a=a,
                b=b,
                n=n,
                c=c,
                m=m,
                tm=tm,
                tr=tr,
                epso=epso,
                cp=cp,
                pc=pc,
                spall=spall,
                d1=d1,
                d2=d2,
                d3=d3,
                d4=d4,
                d5=d5,
                c2_or_erod=c2_or_erod,
            )
        else:
            logging.warning("create_mat_johnson_cook is only available with the keywords backend")
            return None

    def create_eos_linear_polynomial(
        self, eosid, c0=0.0, c1=0.0, c2=0.0, c3=0.0, c4=0.0, c5=0.0, c6=0.0, e0=0.0, v0=0.0
    ):
        """Create an EOS_LINEAR_POLYNOMIAL keyword.

        Parameters
        ----------
        eosid : int
            EOS ID.
        c0-c6 : float, optional
            Polynomial coefficients. The default is ``0.0``.
        e0 : float, optional
            Initial internal energy. The default is ``0.0``.
        v0 : float, optional
            Initial relative volume. The default is ``0.0``.

        Returns
        -------
        int
            The EOS ID.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_eos_linear_polynomial(
                eosid=eosid, c0=c0, c1=c1, c2=c2, c3=c3, c4=c4, c5=c5, c6=c6, e0=e0, v0=v0
            )
        else:
            logging.warning("create_eos_linear_polynomial is only available with the keywords backend")
            return None

    def create_eos_jwl(self, eosid, a=0.0, b=0.0, r1=0.0, r2=0.0, omeg=0.0, e0=0.0, vo=0.0):
        """Create an EOS_JWL keyword.

        Parameters
        ----------
        eosid : int
            EOS ID.
        a : float, optional
            JWL parameter A. The default is ``0.0``.
        b : float, optional
            JWL parameter B. The default is ``0.0``.
        r1 : float, optional
            JWL parameter R1. The default is ``0.0``.
        r2 : float, optional
            JWL parameter R2. The default is ``0.0``.
        omeg : float, optional
            JWL parameter omega. The default is ``0.0``.
        e0 : float, optional
            Initial internal energy. The default is ``0.0``.
        vo : float, optional
            Initial relative volume. The default is ``0.0``.

        Returns
        -------
        int
            The EOS ID.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_eos_jwl(eosid=eosid, a=a, b=b, r1=r1, r2=r2, omeg=omeg, e0=e0, vo=vo)
        else:
            logging.warning("create_eos_jwl is only available with the keywords backend")
            return None

    def create_eos_gruneisen(self, eosid, c=0.0, s1=0.0, s2=0.0, s3=0.0, gamao=0.0, a=0.0, e0=0.0, v0=0.0):
        """Create an EOS_GRUNEISEN keyword.

        Parameters
        ----------
        eosid : int
            EOS ID.
        c : float, optional
            Intercept of shock velocity vs particle velocity curve. The default is ``0.0``.
        s1-s3 : float, optional
            Slope coefficients. The default is ``0.0``.
        gamao : float, optional
            Gruneisen coefficient. The default is ``0.0``.
        a : float, optional
            First order volume correction. The default is ``0.0``.
        e0 : float, optional
            Initial internal energy. The default is ``0.0``.
        v0 : float, optional
            Initial relative volume. The default is ``0.0``.

        Returns
        -------
        int
            The EOS ID.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_eos_gruneisen(
                eosid=eosid, c=c, s1=s1, s2=s2, s3=s3, gamao=gamao, a=a, e0=e0, v0=v0
            )
        else:
            logging.warning("create_eos_gruneisen is only available with the keywords backend")
            return None

    def create_initial_detonation(self, pid, x=0.0, y=0.0, z=0.0, lt=0.0, mmgset=0):
        """Create an INITIAL_DETONATION keyword.

        Parameters
        ----------
        pid : int
            Part ID.
        x : float, optional
            X coordinate of detonation point. The default is ``0.0``.
        y : float, optional
            Y coordinate of detonation point. The default is ``0.0``.
        z : float, optional
            Z coordinate of detonation point. The default is ``0.0``.
        lt : float, optional
            Lighting time. The default is ``0.0``.
        mmgset : int, optional
            Multi-material group set ID. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            self.stub._backend.create_initial_detonation(pid=pid, x=x, y=y, z=z, lt=lt, mmgset=mmgset)
            return True
        else:
            logging.warning("create_initial_detonation is only available with the keywords backend")
            return False

    def create_ale_structured_mesh(
        self, mshid=1, dpid=2, nbid=2000001, ebid=2000001, tdeath=0.0, cpidx=1, cpidy=2, cpidz=3
    ):
        """Create an ALE_STRUCTURED_MESH keyword.

        Parameters
        ----------
        mshid : int, optional
            Mesh ID. The default is ``1``.
        dpid : int, optional
            Default part ID. The default is ``2``.
        nbid : int, optional
            Beginning node ID. The default is ``2000001``.
        ebid : int, optional
            Beginning element ID. The default is ``2000001``.
        tdeath : float, optional
            Death time. The default is ``0.0``.
        cpidx : int, optional
            Control points ID for X. The default is ``1``.
        cpidy : int, optional
            Control points ID for Y. The default is ``2``.
        cpidz : int, optional
            Control points ID for Z. The default is ``3``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            self.stub._backend.create_ale_structured_mesh(
                mshid=mshid,
                dpid=dpid,
                nbid=nbid,
                ebid=ebid,
                tdeath=tdeath,
                cpidx=cpidx,
                cpidy=cpidy,
                cpidz=cpidz,
            )
            return True
        else:
            logging.warning("create_ale_structured_mesh is only available with the keywords backend")
            return False

    def create_ale_structured_mesh_control_points(self, cpid, icase=2, sfo=1.0, offo=0.0, points=None):
        """Create an ALE_STRUCTURED_MESH_CONTROL_POINTS keyword.

        Parameters
        ----------
        cpid : int
            Control points ID.
        icase : int, optional
            Control point case type. The default is ``2``.
        sfo : float, optional
            Scale factor offset. The default is ``1.0``.
        offo : float, optional
            Offset factor offset. The default is ``0.0``.
        points : list, optional
            List of tuples (n, x, ratio) for each control point.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            self.stub._backend.create_ale_structured_mesh_control_points(
                cpid=cpid, icase=icase, sfo=sfo, offo=offo, points=points or []
            )
            return True
        else:
            logging.warning("create_ale_structured_mesh_control_points is only available with the keywords backend")
            return False

    def create_ale_structured_multi_material_group(self, groups=None):
        """Create an ALE_STRUCTURED_MULTI-MATERIAL_GROUP keyword.

        Parameters
        ----------
        groups : list, optional
            List of dicts with keys: ammgnm (name), mid (material ID),
            eosid (EOS ID), pref (reference pressure).

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            self.stub._backend.create_ale_structured_multi_material_group(groups=groups or [])
            return True
        else:
            logging.warning("create_ale_structured_multi_material_group is only available with the keywords backend")
            return False

    def create_ale_structured_mesh_volume_filling(self, mshid, ammgto, nsample=4, geom="ALL", in_out=0, e1=0):
        """Create an ALE_STRUCTURED_MESH_VOLUME_FILLING keyword.

        Parameters
        ----------
        mshid : int
            Mesh ID.
        ammgto : str
            Multi-material group name to fill.
        nsample : int, optional
            Number of sampling points. The default is ``4``.
        geom : str, optional
            Geometry type ("ALL", "PART", etc.). The default is ``"ALL"``.
        in_out : int, optional
            Inside/outside fill flag. The default is ``0``.
        e1 : int, optional
            Geometry element/part ID. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            self.stub._backend.create_ale_structured_mesh_volume_filling(
                mshid=mshid, ammgto=ammgto, nsample=nsample, geom=geom, in_out=in_out, e1=e1
            )
            return True
        else:
            logging.warning("create_ale_structured_mesh_volume_filling is only available with the keywords backend")
            return False

    def create_ale_structured_mesh_refine(self, mshid, ityp=1, idir=1, n1=1, n2=0, ntimes=0):
        """Create an ALE_STRUCTURED_MESH_REFINE keyword.

        Parameters
        ----------
        mshid : int
            Mesh ID.
        ityp : int, optional
            Refinement type. The default is ``1``.
        idir : int, optional
            Refinement direction. The default is ``1``.
        n1 : int, optional
            First parameter. The default is ``1``.
        n2 : int, optional
            Second parameter. The default is ``0``.
        ntimes : int, optional
            Number of refinement times. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.

        Note
        ----
        This method requires the keywords backend. It is not available with the gRPC stub.
        """
        if hasattr(self.stub, "_backend"):
            self.stub._backend.create_ale_structured_mesh_refine(
                mshid=mshid, ityp=ityp, idir=idir, n1=n1, n2=n2, ntimes=ntimes
            )
            return True
        else:
            logging.warning("create_ale_structured_mesh_refine is only available with the keywords backend")
            return False

    def set_termination(self, endtime):
        """Set the time for ending the simulation.

        Parameters
        ----------
        endtime : float
            Time for ending the simulation.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.stub.CreateTermination(TerminationRequest(endtim=endtime))

    def set_output_interval(self, database_plot_interval):
        """Request binary output.

        Parameters
        ----------
        database_plot_interval : float
            Time interval between output states.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.stub.CreateDBBinary(DBBinaryRequest(filetype="D3PLOT", dt=database_plot_interval))

    def set_analysis_type(
        self,
        num_of_cycle=1,
        method=AdvectionMethod.DONOR_CELL_WITH_HALF_INDEX_SHIFT,
        background_pressure=0,
    ):
        """Set the analysis type.

        Parameters
        ----------
        num_of_cycle : float, optional
            Total time of simulation for the fluid problem. The
            default is ``1``.
        method : float, optional
            Time step for the fluid problem. The default is
            ``DONOR_CELL_WITH_HALF_INDEX_SHIFT``.
        background_pressure : int, optional
            Reference pressure for computing the internal forces.
            The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.ALECreateControl(
            ControlALERequest(
                dct=0,
                nadv=num_of_cycle,
                meth=method.value,
                afac=0,
                end=1e20,
                aafac=1,
                vfact=1e-6,
                pref=background_pressure,
            )
        )
        logging.info("Setup Analysis...")
        return ret

    def set_output_database(self, matsum=0, glstat=0):
        """Obtain output files containing the results.

        Parameters
        ----------
        matsum : float, optional
            Time interval between outputs of part energies. The
            default is ``0``.
        glstat : float, optional
            Time interval between outputs of global statistics and energies.
            The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if matsum > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="MATSUM", dt=matsum, binary=1, lcur=0, ioopt=0))
        if glstat > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="GLSTAT", dt=glstat, binary=1, lcur=0, ioopt=0))
        ret = 1
        logging.info("Output Setting...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        self.set_energy(
            hourglass_energy=EnergyFlag.COMPUTED,
            sliding_interface_energy=EnergyFlag.COMPUTED,
        )
        DynaBase.save_file(self)
