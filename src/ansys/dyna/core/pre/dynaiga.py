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
IGA API
==========

Module for creating an IGA DYNA input deck.
"""

import logging

from .dynabase import *  # noqa : F403


class DynaIGA(DynaBase):
    """Contains methods for creating a keyword related to IGA."""

    def __init__(self):
        DynaBase.__init__(self)
        self.casetype = CaseType.IGA

    def create_control_timestep(
        self,
        tssfac=0.9,
        isdo=0,
        tslimt=0.0,
        dt2ms=0.0,
        lctm=0,
        erode=0,
        ms1st=0,
        dt2msf=0.0,
    ):
        """Create a CONTROL_TIMESTEP keyword.

        Parameters
        ----------
        tssfac : float, optional
            Scale factor for computed time step. Default is 0.9.
        isdo : int, optional
            Basis of time step size calculation. Default is 0.
        tslimt : float, optional
            Minimum time step limit. Default is 0.0.
        dt2ms : float, optional
            Time step for mass scaled solutions. Default is 0.0.
            Negative values indicate element size-based mass scaling.
        lctm : int, optional
            Load curve ID for mass scaling limit. Default is 0.
        erode : int, optional
            Erosion flag. Default is 0.
        ms1st : int, optional
            First cycle mass scaling. Default is 0.
        dt2msf : float, optional
            Mass scaling factor. Default is 0.0.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if self.no_grpc:
            from ansys.dyna.core.keywords import keywords

            kw = keywords.ControlTimestep()
            kw.tssfac = tssfac
            kw.isdo = isdo
            kw.tslimt = tslimt
            kw.dt2ms = dt2ms
            kw.lctm = lctm
            kw.erode = erode
            kw.ms1st = ms1st
            kw.dt2msf = dt2msf
            self.stub._backend._deck.append(kw)
            logging.info("Control Timestep Created...")
            return True
        else:
            from ansys.dyna.core.pre.dynamaterial_pb2 import ControlTimestepRequest

            ret = self.stub.CreateControlTimestep(
                ControlTimestepRequest(
                    tssfac=tssfac,
                    isdo=isdo,
                    tslimt=tslimt,
                    dt2ms=dt2ms,
                    lctm=lctm,
                    erode=erode,
                    ms1st=ms1st,
                    dt2msf=dt2msf,
                )
            )
            logging.info("Control Timestep Created...")
            return ret

    def create_rigidwall_cylinder(
        self,
        xt=0.0,
        yt=0.0,
        zt=0.0,
        xh=0.0,
        yh=0.0,
        zh=0.0,
        fric=0.0,
        radcyl=1.0,
        lencyl=10.0,
        nsid=0,
        nsidex=0,
        boxid=0,
        birth=0.0,
        death=0.0,
        nsegs=0,
        pid=0,
        ro=1.0e-9,
        e=1.0e-4,
        pr=0.3,
    ):
        """Create a RIGIDWALL_GEOMETRIC_CYLINDER_DISPLAY keyword.

        Parameters
        ----------
        xt, yt, zt : float, optional
            Coordinates of the tail of the cylinder axis. Defaults are 0.0.
        xh, yh, zh : float, optional
            Coordinates of the head of the cylinder axis. Defaults are 0.0.
        fric : float, optional
            Friction coefficient. Default is 0.0.
        radcyl : float, optional
            Radius of the cylinder. Default is 1.0.
        lencyl : float, optional
            Length of the cylinder. Default is 10.0.
        nsid : int, optional
            Node set ID for interacting nodes. Default is 0 (all).
        nsidex : int, optional
            Node set ID for excluded nodes. Default is 0.
        boxid : int, optional
            Box ID defining interacting nodes. Default is 0.
        birth : float, optional
            Birth time. Default is 0.0.
        death : float, optional
            Death time. Default is 0.0.
        nsegs : int, optional
            Number of segments for display. Default is 0.
        pid : int, optional
            Part ID for display. Default is 0.
        ro : float, optional
            Density for display. Default is 1.0e-9.
        e : float, optional
            Young's modulus for display. Default is 1.0e-4.
        pr : float, optional
            Poisson's ratio for display. Default is 0.3.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if self.no_grpc:
            from ansys.dyna.core.keywords import keywords

            kw = keywords.RigidwallGeometricCylinderDisplay()
            kw.nsid = nsid
            kw.nsidex = nsidex
            kw.boxid = boxid
            kw.birth = birth
            kw.death = death
            kw.xt = xt
            kw.yt = yt
            kw.zt = zt
            kw.xh = xh
            kw.yh = yh
            kw.zh = zh
            kw.fric = fric
            kw.radcyl = radcyl
            kw.lencyl = lencyl
            kw.nsegs = nsegs
            kw.pid = pid
            kw.ro = ro
            kw.e = e
            kw.pr = pr
            self.stub._backend._deck.append(kw)
            logging.info("Rigidwall Cylinder Created...")
            return True
        else:
            # Fall back to gRPC stub
            parameter = [xt, yt, zt, xh, yh, zh, radcyl, lencyl]
            self.stub.CreateRigidWallGeom(
                RigidWallGeomRequest(
                    geomtype=3,
                    motion=-1,
                    display=1,
                    parameter=parameter,
                    fric=fric,
                )
            )
            logging.info("Rigidwall Cylinder Created...")
            return True

    def create_rigidwall_cylinder_motion(
        self,
        xt=0.0,
        yt=0.0,
        zt=0.0,
        xh=0.0,
        yh=0.0,
        zh=0.0,
        fric=0.0,
        radcyl=1.0,
        lencyl=10.0,
        lcid=0,
        opt=0,
        vx=0.0,
        vy=0.0,
        vz=0.0,
        nsid=0,
        nsidex=0,
        boxid=0,
        birth=0.0,
        death=0.0,
        nsegs=0,
        pid=0,
        ro=1.0e-9,
        e=1.0e-4,
        pr=0.3,
    ):
        """Create a RIGIDWALL_GEOMETRIC_CYLINDER_MOTION_DISPLAY keyword.

        Parameters
        ----------
        xt, yt, zt : float, optional
            Coordinates of the tail of the cylinder axis. Defaults are 0.0.
        xh, yh, zh : float, optional
            Coordinates of the head of the cylinder axis. Defaults are 0.0.
        fric : float, optional
            Friction coefficient. Default is 0.0.
        radcyl : float, optional
            Radius of the cylinder. Default is 1.0.
        lencyl : float, optional
            Length of the cylinder. Default is 10.0.
        lcid : int, optional
            Load curve ID for motion. Default is 0.
        opt : int, optional
            Motion option. Default is 0.
        vx, vy, vz : float, optional
            Direction vector for motion. Defaults are 0.0.
        nsid : int, optional
            Node set ID for interacting nodes. Default is 0 (all).
        nsidex : int, optional
            Node set ID for excluded nodes. Default is 0.
        boxid : int, optional
            Box ID defining interacting nodes. Default is 0.
        birth : float, optional
            Birth time. Default is 0.0.
        death : float, optional
            Death time. Default is 0.0.
        nsegs : int, optional
            Number of segments for display. Default is 0.
        pid : int, optional
            Part ID for display. Default is 0.
        ro : float, optional
            Density for display. Default is 1.0e-9.
        e : float, optional
            Young's modulus for display. Default is 1.0e-4.
        pr : float, optional
            Poisson's ratio for display. Default is 0.3.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        if self.no_grpc:
            from ansys.dyna.core.keywords import keywords

            kw = keywords.RigidwallGeometricCylinderMotionDisplay()
            kw.nsid = nsid
            kw.nsidex = nsidex
            kw.boxid = boxid
            kw.birth = birth
            kw.death = death
            kw.xt = xt
            kw.yt = yt
            kw.zt = zt
            kw.xh = xh
            kw.yh = yh
            kw.zh = zh
            kw.fric = fric
            kw.radcyl = radcyl
            kw.lencyl = lencyl
            kw.nsegs = nsegs
            kw.lcid = lcid
            kw.opt = opt
            kw.vx = vx
            kw.vy = vy
            kw.vz = vz
            kw.pid = pid
            kw.ro = ro
            kw.e = e
            kw.pr = pr
            self.stub._backend._deck.append(kw)
            logging.info("Rigidwall Cylinder Motion Created...")
            return True
        else:
            # Fall back to gRPC stub
            parameter = [xt, yt, zt, xh, yh, zh, radcyl, lencyl]
            self.stub.CreateRigidWallGeom(
                RigidWallGeomRequest(
                    geomtype=3,
                    motion=opt,
                    display=1,
                    parameter=parameter,
                    lcid=lcid,
                    vx=vx,
                    vy=vy,
                    vz=vz,
                    fric=fric,
                )
            )
            logging.info("Rigidwall Cylinder Motion Created...")
            return True

    def create_section_igashell(self, secid, elform, shrf, thickness):
        """Define section properties for isogeometric shell elements.

        Parameters
        ----------
        secid : int
            Section ID. ``SECID`` is referenced on the ``\*PART`` card.
            A unique number or label must be specified.
        elform : int
            Element formulation.
        shrf : float
            Shear correction factor, which scales the transverse shear stress.
        thickness : float
            Shell thickness.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        ret = self.stub.CreateSectionIGAShell(
            SectionIGAShellRequest(secid=secid, elform=elform, shrf=shrf, thickness=thickness)
        )
        logging.info("Section IGAShell 1 Created...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        DynaBase.save_file(self)
        self.create_control_contact(rwpnal=1.0, ignore=1, igactc=1)
