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
NVH API
==========

Module for creating an NVH Dyna input deck.
"""

import logging

from .dynabase import *  # noqa : F403


class DynaNVH(DynaBase):
    """Contains methods for creating a keyword related to NVH."""

    def __init__(self):
        DynaBase.__init__(self)

    def create_frequency_domain_frf(
        self,
        n1=0,
        n1typ=0,
        dof1=1,
        vad1=1,
        fnmax=0.0,
        dampf=0.0,
        lcdam=0,
        lctyp=0,
        n2=0,
        n2typ=1,
        dof2=1,
        vad2=1,
        fmin=0.0,
        fmax=0.0,
        nfreq=100,
    ):
        """Create a FREQUENCY_DOMAIN_FRF keyword.

        Parameters
        ----------
        n1 : int, optional
            Input node/set ID. Default is 0.
        n1typ : int, optional
            Input type (0=node, 1=node set, 2=segment set). Default is 0.
        dof1 : int, optional
            Input DOF (0=vector, 1=X, 2=Y, 3=Z). Default is 1.
        vad1 : int, optional
            Input type (0=velocity, 1=acceleration, 2=displacement, 3=force). Default is 1.
        fnmax : float, optional
            Maximum natural frequency. Default is 0.0.
        dampf : float, optional
            Modal damping coefficient. Default is 0.0.
        lcdam : int, optional
            Load curve ID for damping vs. frequency. Default is 0.
        lctyp : int, optional
            Load curve type (0=damping ratio, 1=damping coefficient). Default is 0.
        n2 : int, optional
            Output node/set ID. Default is 0.
        n2typ : int, optional
            Output type (0=node, 1=node set, 2=segment set). Default is 1.
        dof2 : int, optional
            Output DOF. Default is 1.
        vad2 : int, optional
            Output type. Default is 1.
        fmin : float, optional
            Minimum frequency for output. Default is 0.0.
        fmax : float, optional
            Maximum frequency for output. Default is 0.0.
        nfreq : int, optional
            Number of frequency points. Default is 100.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_frequency_domain_frf(
                n1=n1,
                n1typ=n1typ,
                dof1=dof1,
                vad1=vad1,
                fnmax=fnmax,
                dampf=dampf,
                lcdam=lcdam,
                lctyp=lctyp,
                n2=n2,
                n2typ=n2typ,
                dof2=dof2,
                vad2=vad2,
                fmin=fmin,
                fmax=fmax,
                nfreq=nfreq,
            )
        else:
            from ansys.dyna.core.pre.dynamech_pb2 import FrequencyDomainFRFRequest

            self.stub.CreateFrequencyDomainFRF(
                FrequencyDomainFRFRequest(
                    n1=n1,
                    n1typ=n1typ,
                    dof1=dof1,
                    vad1=vad1,
                    fnmax=fnmax,
                    dampf=dampf,
                    lcdam=lcdam,
                    lctyp=lctyp,
                    n2=n2,
                    n2typ=n2typ,
                    dof2=dof2,
                    vad2=vad2,
                    fmin=fmin,
                    fmax=fmax,
                    nfreq=nfreq,
                )
            )
            return True

    def create_boundary_spc_set(
        self,
        nsid,
        cid=0,
        dofx=0,
        dofy=0,
        dofz=0,
        dofrx=0,
        dofry=0,
        dofrz=0,
    ):
        """Create a BOUNDARY_SPC_SET keyword.

        Parameters
        ----------
        nsid : int
            Node set ID.
        cid : int, optional
            Coordinate system ID. Default is 0.
        dofx : int, optional
            Constrain X translation (0=free, 1=fixed). Default is 0.
        dofy : int, optional
            Constrain Y translation. Default is 0.
        dofz : int, optional
            Constrain Z translation. Default is 0.
        dofrx : int, optional
            Constrain X rotation. Default is 0.
        dofry : int, optional
            Constrain Y rotation. Default is 0.
        dofrz : int, optional
            Constrain Z rotation. Default is 0.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_boundary_spc_set(
                nsid=nsid,
                cid=cid,
                dofx=dofx,
                dofy=dofy,
                dofz=dofz,
                dofrx=dofrx,
                dofry=dofry,
                dofrz=dofrz,
            )
        else:
            from ansys.dyna.core.pre.dynamech_pb2 import BoundarySPCSetRequest

            self.stub.CreateBoundarySpcSet(
                BoundarySPCSetRequest(
                    nsid=nsid,
                    cid=cid,
                    dofx=dofx,
                    dofy=dofy,
                    dofz=dofz,
                    dofrx=dofrx,
                    dofry=dofry,
                    dofrz=dofrz,
                )
            )
            return True

    def create_database_glstat(self, dt=0.0, binary=1, lcur=0, ioopt=0):
        """Create a DATABASE_GLSTAT keyword.

        Parameters
        ----------
        dt : float, optional
            Time interval between outputs. Default is 0.0.
        binary : int, optional
            Binary file flag. Default is 1.
        lcur : int, optional
            Load curve ID. Default is 0.
        ioopt : int, optional
            Output option. Default is 0.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_database_glstat(dt=dt, binary=binary, lcur=lcur, ioopt=ioopt)
        else:
            from ansys.dyna.core.pre.dynamech_pb2 import DatabaseGlstatRequest

            self.stub.CreateDatabaseGlstat(DatabaseGlstatRequest(dt=dt, binary=binary, lcur=lcur, ioopt=ioopt))
            return True

    def create_database_matsum(self, dt=0.0, binary=1, lcur=0, ioopt=0):
        """Create a DATABASE_MATSUM keyword.

        Parameters
        ----------
        dt : float, optional
            Time interval between outputs. Default is 0.0.
        binary : int, optional
            Binary file flag. Default is 1.
        lcur : int, optional
            Load curve ID. Default is 0.
        ioopt : int, optional
            Output option. Default is 0.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_database_matsum(dt=dt, binary=binary, lcur=lcur, ioopt=ioopt)
        else:
            from ansys.dyna.core.pre.dynamech_pb2 import DatabaseMatsumRequest

            self.stub.CreateDatabaseMatsum(DatabaseMatsumRequest(dt=dt, binary=binary, lcur=lcur, ioopt=ioopt))
            return True

    def create_section_solid(self, secid, elform=1):
        """Create a SECTION_SOLID keyword.

        Parameters
        ----------
        secid : int
            Section ID.
        elform : int, optional
            Element formulation. Default is 1.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if hasattr(self.stub, "_backend"):
            return self.stub._backend.create_section_solid(secid=secid, elform=elform)
        else:
            from ansys.dyna.core.pre.dynamech_pb2 import SectionSolidRequest

            self.stub.CreateSectionSolid(SectionSolidRequest(secid=secid, elform=elform))
            return True

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        DynaBase.save_file(self)


class ExcitationDOF(Enum):
    VECTOR = 0
    X = 1
    Y = 2
    Z = 3


class ExcitationType(Enum):
    BASE_VELOCITY = 0
    BASE_ACCELERATION = 1
    BASE_DISPLACEMENT = 2
    NODAL_FORCE = 3


class ResponseDOF(Enum):
    VECTOR = 0
    X = 1
    Y = 2
    Z = 3


class ResponseType(Enum):
    BASE_VELOCITY = 0
    BASE_ACCELERATION = 1
    BASE_DISPLACEMENT = 2
    NODAL_FORCE = 3


class FrequencyDomain(BaseObj):
    """Provides a way of defining and solving frequency domain vibration and acoustic problems."""

    def __init__(self):
        self.stub = DynaBase.get_stub()
        self.defined_frf = False
        self.type = "frequency_domain"

    def set_frequency_response_function(
        self,
        excitation_input_set=None,
        excitation_input_dof=ExcitationDOF.VECTOR,
        excitation_input_type=ExcitationType.NODAL_FORCE,
        max_natural_frequency=0,
        modal_damping_coefficient=0,
        modal_damping_coefficient_curve=None,
        modal_damping_coefficient_curve_type=0,
        response_output_set=None,
        response_output_dof=ResponseDOF.Y,
        response_output_type=ResponseType.BASE_VELOCITY,
        frf_output_min_frequency=0,
        frf_output_max_frequency=0,
        frf_output_num_frequency=0,
    ):
        """Compute frequency response functions due to nodal excitations.

        Parameters
        ----------
        excitation_input_set :
        excitation_input_dof :
        excitation_input_type :
        max_natural_frequency :
        modal_damping_coefficient :
        modal_damping_coefficient_curve :
        modal_damping_coefficient_curve_type :
        response_output_set :
        response_output_dof :
        response_output_type :
        frf_output_min_frequency :
        frf_output_max_frequency :
        frf_output_num_frequency :

        """
        self.defined_frf = True
        self.n1 = excitation_input_set
        self.dof1 = excitation_input_dof.value
        self.vad1 = excitation_input_type.value
        self.fnmax = max_natural_frequency
        self.dampf = modal_damping_coefficient
        self.lcdam = modal_damping_coefficient_curve
        self.lctyp = modal_damping_coefficient_curve_type
        self.n2 = response_output_set
        self.dof2 = response_output_dof.value
        self.vad2 = response_output_type.value
        self.fmin = frf_output_min_frequency
        self.fmax = frf_output_max_frequency
        self.nfreq = frf_output_num_frequency

    def create(self):
        """Define a frequency domain vibration and acoustic problem."""
        if self.defined_frf:
            if self.lcdam is not None:
                cid = self.lcdam.create(self.stub)
            else:
                cid = 0
            n1typ, n1id = 0, 0
            if self.n1 is not None:
                self.n1.create(self.stub)
                if self.n1.type.upper() == "NODE":
                    n1typ = 0
                    n1id = self.n1.get_nid()
                elif self.n1.type.upper() == "NODESET":
                    n1typ = 1
                    n1id = self.n1.id
                elif self.n1.typ.upper() == "SEGMENTSET":
                    n1typ = 2
                    n1id = self.n1.id
                else:
                    print("Invalid set type.")
            n2typ, n2id = 0, 0
            if self.n2 is not None:
                self.n2.create(self.stub)
                if self.n2.type.upper() == "NODE":
                    n2typ = 0
                    n2id = self.n2.get_nid()
                elif self.n2.type.upper() == "NODESET":
                    n2typ = 1
                    n2id = self.n2.id
                elif self.n2.typ.upper() == "SEGMENTSET":
                    n2typ = 2
                    n2id = self.n2.id
                else:
                    print("Invalid set type.")

            self.stub.CreateFrequencyDomainFRF(
                FrequencyDomainFRFRequest(
                    n1=n1id,
                    n1typ=n1typ,
                    dof1=self.dof1,
                    vad1=self.vad1,
                    fnmax=self.fnmax,
                    dampf=self.dampf,
                    lcdam=cid,
                    lctyp=self.lctyp,
                    n2=n2id,
                    n2typ=n2typ,
                    dof2=self.dof2,
                    vad2=self.vad2,
                    fmin=self.fmin,
                    fmax=self.fmax,
                    nfreq=self.nfreq,
                )
            )
            logging.info("Frequency response function Created...")
