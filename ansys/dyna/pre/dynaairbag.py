"""Module to create Airbag dyna input deck"""

import logging

from .dynabase import *


class DynaAirbag(DynaBase):
    """Contains methods to create keyword related to airbag"""

    def __init__(self, hostname = 'localhost'):
        DynaBase.__init__(self, hostname)

    def create_simple_airbag_model(
        self, modeltype, sid, sidtyp, cv, cp, t,lcid,mu,area,pe,ro
    ):
        """Define an airbag or control volume.
        Refer to: *SIMPLE_AIRBAG_MODEL
        Parameters
        ----------
        modeltype : string
            specifies one of the following thermodynamic models for modeling airbags using a control volume (CV) approach.
            "SIMPLE_AIRBAG_MODEL"
        sid : int
            Set ID.
        sidtyp : int
            Set type: EQ.0:segment EQ.1: part set ID.
        cv : float
            Heat capacity at constant volume.
        cp : float
            Heat capacity at constant pressure.
        t : float
            Temperature of input gas.
        lcid : int
            Load curve ID specifying input mass flow rate.
        mu : float
            Shape factor for exit hole.
        area : float
            Exit area.
        pe : float
            Ambient pressure.
        ro : float
            Ambient density.
        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateAirbagModel(
            AirbagModelRequest(
                modeltype=modeltype,
                sid=sid,
                sidtyp=sidtyp,
                cv=cv,
                cp=cp,
                t=t,
                lcid = lcid,
                mu = mu,
                area = area,
                pe = pe,
                ro = ro
            )
        )
        logging.info("Airbag Model Created...")
        return ret

