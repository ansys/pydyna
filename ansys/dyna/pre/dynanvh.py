"""
NVH API
==========

Module to create NVH dyna input deck
"""

from email.utils import decode_rfc2231
import logging

from .dynabase import *  # noqa : F403


class DynaNVH(DynaBase):
    """Contains methods to create keyword related to NVH."""

    def __init__(self):
        DynaBase.__init__(self)

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        DynaBase.save_file(self)

class FrequencyDomain:
    """Provide a way of defining and solving frequency domain vibration and acoustic problems."""

    def __init__(self):
        self.stub = DynaBase.get_stub()
        self.defined_frf = False 

    def set_frequency_response_function(self, 
    excitation_input_set=None,
    excitation_input_dof=0,
    excitation_input_type=3,
    max_natural_frequency=0,
    modal_damping_coefficient = 0,
    modal_damping_coefficient_curve = 0,
    modal_damping_coefficient_curve_type = 0,
    response_output_set=None,
    response_output_dof=2,
    response_output_type=0,
    frf_output_min_frequency =0,
    frf_output_max_frequency = 0,
    frf_output_num_frequency = 0
    ):
        """Computes frequency response functions due to nodal excitations.

        Parameters
        ----------
        box : Box
            When l save eract with the structure.
        """
        self.defined_frf = True
        self.n1 = excitation_input_set 
        self.dof1 = excitation_input_dof
        self.vad1 = excitation_input_type
        self.fnmax = max_natural_frequency
        self.dampf = modal_damping_coefficient
        self.lcdam = modal_damping_coefficient_curve
        self.lctyp = modal_damping_coefficient_curve_type
        self.n2 = response_output_set
        self.dof2 = response_output_dof
        self.vad2 = response_output_type
        self.fmin = frf_output_min_frequency
        self.fmax = frf_output_max_frequency
        self.nfreq = frf_output_num_frequency

    def create(self):
        """Define frequency domain vibration and acoustic problems."""
        if self.defined_frf:
            self.n1.create(self.stub)
            self.n2.create(self.stub)
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
                    n1typ = n1typ,
                    dof1 = self.dof1,
                    vad1 = self.vad1,
                    fnmax=self.fnmax,
                    dampf=self.dampf,
                    lcdam=self.lcdam,
                    lctyp=self.lctyp,
                    n2=n2id,
                    n2typ = n2typ,
                    dof2 = self.dof2,
                    vad2 = self.vad2,
                    fmin = self.fmin,
                    fmax = self.fmax,
                    nfreq = self.nfreq
                )
            )
            logging.info("Frequency response function Created...")