"""Module to create dyna input deck"""

import os
import grpc
import sys
import logging

"""
import kwprocess_pb2
import kwprocess_pb2_grpc
"""

from .kwprocess_pb2 import *
from .kwprocess_pb2_grpc import *

CHUNK_SIZE = 1024 * 1024


def get_file_chunks(filename):
    with open(filename, "rb") as f:
        while True:
            piece = f.read(CHUNK_SIZE)
            if len(piece) == 0:
                return
            yield Chunk(buffer=piece)


def upload(stub_, filename):
    chunks_generator = get_file_chunks(filename)
    response = stub_.Upload(chunks_generator)


def download(stub_, remote_name, local_name):
    response = stub_.Download(DownloadRequest(url=remote_name))
    with open(local_name, "wb") as f:
        for chunk in response:
            f.write(chunk.buffer)


def init_log(log_file):
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(asctime)s : %(levelname)s  %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
        filename=log_file,
        filemode="w",
    )
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    formatter = logging.Formatter(
        "%(asctime)s :  %(message)s", datefmt="%Y-%m-%d %H:%M:%S"
    )
    console.setFormatter(formatter)
    logging.getLogger().addHandler(console)


class DynaBase:
    """Contains methods to create general LS-DYNA keyword"""

    def __init__(self):
        init_log("client.log")
        hostname = "localhost"
        if len(sys.argv) > 1:
            hostname = sys.argv[1]
        channel = grpc.insecure_channel(hostname + ":50051")
        try:
            grpc.channel_ready_future(channel).result(timeout=5)
        except grpc.FutureTimeoutError:
            logging.critical("Can not connect to kwServer")
            sys.exit()
        logging.info("Connected to kwServer...")
        self.stub = kwC2SStub(channel)
        self.mainname = ""

    def open_files(self, filenames):
        """Open IGA model files
        Parameters
        ----------
        filenames : list
            filenames[0] is the main file,the others are subfile.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        path = os.getcwd()
        for filename in filenames:
            fn = os.path.basename(filename)
            self.stub.kwSetFileName(kwFileName(name=fn, num=filenames.index(filename)))
            upload(self.stub, path + os.sep + "input" + os.sep + fn)
            logging.info(
                path + os.sep + "input" + os.sep + fn + " uploaded to server..."
            )

        self.mainname = os.path.basename(filenames[0])
        return self.stub.LoadFile(LoadFileRequest())

    def create_timestep(self, tssfac, isdo, dt2ms):
        """Create *CONTROL_TIMESTEP keyword
        Parameters
        ----------
        tssfac : float
            Scale factor for computed time step.
        isdo : int
            Basis of time size calculation for 4-node shell elements.
        dt2ms : float
            Time step size for mass scaled solutions.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateTimestep(
            TimestepRequest(tssfac=tssfac, isdo=isdo, dt2ms=dt2ms)
        )
        logging.info("Timestep Created...")
        return ret

    def create_termination(self, endtim):
        """Create *CONTROL_TERMINATION keyword
        Parameters
        ----------
        endtim : float
            Termination time.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateTermination(TerminationRequest(endtim=endtim))
        logging.info("Termination Created...")
        return ret

    def create_contact(self, rwpnal, ignore, igactc):
        """Create *CONTROL_CONTACT keyword
        Parameters
        ----------
        rwpnal : float
            Scale factor for rigid wall penalties, which treat nodal points interacting with rigid walls.
        ignore : int
            Ignore initial penetrations in the *CONTACT_AUTOMATIC options.
        igactc : int
            Options to use isogeometric shells for contact detection when contact involves isogeometric shells.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateContact(
            ContactRequest(rwpnal=rwpnal, ignore=ignore, igactc=igactc)
        )
        logging.info("Contact Created...")
        return ret

    def create_database_binary(self, dt):
        """Create *DATABASE_BINARY_D3PLOT keyword
        Parameters
        ----------
        dt : float
            Defines the time interval between output states.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDBBinary(DBBinaryRequest(dt=dt))
        logging.info("DB Binary Created...")
        return ret

    def create_rigidwall_geom(
        self, geomtype, motion, display, parameter, lcid, vx, vy, vz
    ):
        """Create *DATABASE_BINARY_D3PLOT keyword
        Parameters
        ----------
        geomtype : int
            The available shape variants are FLAT, PRISM, CYLINDER and SPHERE.
        motion : int
            If prescribed motion is desired an additional option is available.
        display : int
            To view the rigid wall, this option is available.
        parameter : list
            x,y,z-coordinate of tail of normal vector n,x,y,z-coordinate of head of normal vector n,radius of cylinder and length of cylinder.
        lcid : int
            if motion defined,this is Rigidwall motion curve ID.
        vx vy vz : float
            if motion defined, these are x,y,z-direction cosine of velocity/displacement vector.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateRigidWallGeom(
            RigidWallGeomRequest(
                geomtype=geomtype,
                motion=motion,
                display=display,
                parameter=parameter,
                lcid=lcid,
                vx=vx,
                vy=vy,
                vz=vz,
            )
        )
        logging.info("Cylinder Rigidwall Geometric Created...")
        return ret

    def create_definecurve(self, lcid, sfo, abscissa, ordinate):
        """Create *DEFINE_CURVE keyword
        Parameters
        ----------
        lcid : int
            Load curve identification.
        sfo : float
            Scale factor for ordinate value.
        abscissa : list
            Abscissa values.
        ordinate : list
            Ordinate (function) values.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDefineCurve(
            DefineCurveRequest(lcid=lcid, sfo=sfo, abscissa=abscissa, ordinate=ordinate)
        )
        logging.info("DefineCurve Created...")
        return ret

    def set_partproperty(self, pid, secid, mid, eosid, hgid, grav, adpopt, tmid):
        """Reset property for *PART keyword
        Parameters
        ----------
        pid : int
            Part identification. A unique number must be specified..
        secid : int
            Section identification defined in a *SECTION keyword.
        mid : int
            Material identification defined in the *MAT section.
        eosid : int
            Equation of state identification defined in the *EOS section.
        hgid : int
            Hourglass/bulk viscosity identification defined in the *HOURGLASS Section.
        grav : int
            Flag to turn on gravity initialization according to *LOAD_DENSITY_DEPTH.
        adpopt : int
            Indicate if this part is adapted or not.
        tmid : int
            Thermal material property identification defined in the *MAT_THERMAL Section.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.SetPartProperty(
            PartPropertyRequest(
                pid=pid,
                secid=secid,
                mid=mid,
                eosid=eosid,
                hgid=hgid,
                grav=grav,
                adpopt=adpopt,
                tmid=tmid,
            )
        )
        return ret

    def create_partset(self, sid, pids):
        """Create *PART keyword
        Parameters
        ----------
        sid : int
            Set ID. All part sets should have a unique set ID.
        pids : list
            A list of part ID.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreatePartSet(PartSetRequest(sid=sid, pids=pids))
        logging.info("PartSet Created...")
        return ret

    def create_section_solid(self, secid, elform):
        """Create *SECTION_SOLID keyword
        Parameters
        ----------
        secid : int
            Section ID. SECID is referenced on the *PART card. A unique number must be specified.
        elform : int
            Element formulation options.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateSectionSolid(
            SectionSolidRequest(secid=secid, elform=elform)
        )
        logging.info("Section Solid Created...")
        return ret

    def create_hourglass(self, ghid, ihq, qm, q1, q2, qb, qw):
        """Create *HOURGLASS keyword
        Parameters
        ----------
        ghid : int
            Hourglass ID. A unique number or label must be specified.
        ihq : int
            Hourglass control type.
        qm : float
            Hourglass coefficient.
        q1 : float
            Quadratic bulk viscosity coefficient.
        q2 : float
            Linear bulk viscosity coefficient.
        qb : float
            Hourglass coefficient for shell bending.
        qw : float
            Hourglass coefficient for shell warping.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateHourglass(
            HourglassRequest(ghid=ghid, ihq=ihq, qm=qm, q1=q1, q2=q2, qb=qb, qw=qw)
        )
        logging.info("Hourglass 1 Created...")
        return ret

    def create_contact_automatic(self, ssid, msid, sstyp, mstyp, option):
        """Create *CONTACT_AUTOMATIC_SINGLE_SURFACE keyword
        Parameters
        ----------
        ssid : int
            Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface.
        msid : int
            Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact.
        sstyp : int
            The ID type of SURFA.
        mstyp : int
            ID type of SURFB.
        option : int
            Soft constraint option.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateContactAutomatic(
            ContactAutomaticRequest(
                ssid=ssid, msid=msid, sstyp=sstyp, mstyp=mstyp, option=option
            )
        )
        logging.info("Contact Automatic  Created...")
        return ret

    def create_contact_tied(self, ssid, msid, sstyp, mstyp):
        """Create *CONTACT_TIED_SHELL_EDGE_TO_SURFACE_OFFSET keyword
        Parameters
        ----------
        ssid : int
            Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface.
        msid : int
            Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact.
        sstyp : int
            The ID type of SURFA.
        mstyp : int
            ID type of SURFB.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateContactTied(
            ContactTiedRequest(ssid=ssid, msid=msid, sstyp=sstyp, mstyp=mstyp)
        )
        logging.info("Contact Tied  Created...")
        return ret

    def get_solid_elements(self):
        """Get solid elements.

        Returns
        -------
        list
            list[0],solid element connectivity,list[0] = [[n1,n2,n3,n4,n5,n6,n7,n8],[...],...]
            list[1],node coordinates,list[1] = [[x1,y1,z1],[x2,y2,z2],...] 
        """
        cons = self.stub.GetSolidElements(GetSolidElementsRequest())
        nodes = self.stub.GetNodes(GetNodesRequest())
        num = 8
        lscons = cons.nodeids
        elist = ([lscons[i:i + num] for i in range(0, len(lscons), num)])
        numconn = 3
        lsnodes = nodes.coords
        nlist = ([lsnodes[i:i + numconn] for i in range(0, len(lsnodes), numconn)])
        elements = [elist,nlist]
        return elements

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.SaveFile(SaveFileRequest(name=self.mainname))
        msg = self.mainname + " is outputed..."
        logging.info(msg)
        return ret
