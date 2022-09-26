"""
Solution
==========

Top object to setup a Dyna Solution
"""

import logging
import os
import sys
from enum import Enum

# from subprocess import DETACHED_PROCESS
import grpc

"""
import kwprocess_pb2
import kwprocess_pb2_grpc
"""

from .kwprocess_pb2 import *  # noqa : F403
from .kwprocess_pb2_grpc import *  # noqa : F403

CHUNK_SIZE = 1024 * 1024


def get_file_chunks(filename):
    """Get file chunks."""
    with open(filename, "rb") as f:
        while True:
            piece = f.read(CHUNK_SIZE)
            if len(piece) == 0:
                return
            yield Chunk(buffer=piece)


def upload(stub_, filename):
    """Upload files to server."""
    chunks_generator = get_file_chunks(filename)
    response = stub_.Upload(chunks_generator)


def download(stub_, remote_name, local_name):
    """Download files from server."""
    response = stub_.Download(DownloadRequest(url=remote_name))
    with open(local_name, "wb") as f:
        for chunk in response:
            f.write(chunk.buffer)


def init_log(log_file):
    """Initial log file."""
    if not logging.getLogger().handlers:
        logging.basicConfig(
            level=logging.DEBUG,
            format="%(asctime)s : %(levelname)s  %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S",
            filename=log_file,
            filemode="w",
        )
        console = logging.StreamHandler()
        console.setLevel(logging.INFO)
        formatter = logging.Formatter("%(asctime)s :  %(message)s", datefmt="%Y-%m-%d %H:%M:%S")
        console.setFormatter(formatter)
        logging.getLogger().addHandler(console)

class DynaSolution:
    """Contains methods to create general LS-DYNA keyword."""

    object_list = []

    def __init__(self, hostname="localhost"):
        init_log("client.log")
        channel = grpc.insecure_channel(hostname + ":50051")
        try:
            grpc.channel_ready_future(channel).result(timeout=5)
        except grpc.FutureTimeoutError:
            logging.critical("Can not connect to kwServer")
            sys.exit()
        logging.info("Connected to kwServer...")
        self.stub = kwC2SStub(channel)
        self.mainname = ""
        DynaSolution.stub = self.stub

    def get_stub():
        """Get the stub of this Solution object."""
        return DynaSolution.stub

    def add(self, obj):
        """Add case in the solution."""
        self.object_list.append(obj)

    def open_files(self, filenames):
        """Open initial model files.

        Parameters
        ----------
        filenames : list
            filenames[0] is the main file,the others are subfile.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        splitfiles = os.path.split(filenames[0])
        path = splitfiles[0]
        for filename in filenames:
            fn = os.path.basename(filename)
            self.stub.kwSetFileName(kwFileName(name=fn, num=filenames.index(filename)))
            upload(self.stub, path + os.sep + fn)
            logging.info(path + os.sep + "input" + os.sep + fn + " uploaded to server...")

        self.mainname = os.path.basename(filenames[0])
        return self.stub.LoadFile(LoadFileRequest())

    def set_termination(self, termination_time):
        """Setting termination time to stop the job.

        Parameters
        ----------
        termination_time : float
            Termination time.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateTermination(TerminationRequest(endtim=termination_time))
        logging.info("Setting termination time ...")
        return ret

    def create_database_binary(self, filetype="D3PLOT", dt=0, maxint=3, ieverp=0, dcomp=1, nintsld=1):
        """Request binary output.

        Parameters
        ----------
        dt : float
            Defines the time interval between output states.
        maxint : int
            Number of shell and thick shell through-thickness integration points
            for which output is written to d3plot.
        ieverp : int
            Every output state for the d3plot database is written to a separate file.
            EQ.0: More than one state can be on each plot file.
            EQ.1: One state only on each plot file.
        dcomp : int
            Data compression to eliminate rigid body data.
        nintsld : int
            Number of solid element integration points written to the LS-DYNA database.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDBBinary(
            DBBinaryRequest(
                filetype=filetype,
                dt=dt,
                maxint=maxint,
                ieverp=ieverp,
                dcomp=dcomp,
                nintsld=nintsld,
            )
        )
        logging.info("DB Binary Created...")
        return ret

    def create_database_ascii(self, type, dt=0.0, binary=1, lcur=0, ioopt=0):
        """Obtain output files containing results information.

        Parameters
        ----------
        type : string
            Specifies the type of database.(BNDOUT,GLSTAT,MATSUM,NODFOR,RCFORC,SLEOUT)
        dt : float
            Time interval between outputs
        binary : int
            Flag for binary output.
        lcur : int
            Optional curve ID specifying time interval between outputs.
        ioopt : int
            Flag to govern behavior of the output frequency load curve defined by LCUR.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        ret = self.stub.CreateDBAscii(DBAsciiRequest(type=type, dt=dt, binary=binary, lcur=lcur, ioopt=ioopt))
        logging.info("DB Ascii Created...")
        return ret

    def set_output_database(
        self,
        matsum=0,
        glstat=0,
        elout=0,
        nodout=0,
        nodfor=0,
        rbdout=0,
        rcforc=0,
        secforc=0,
        rwforc=0,
        abstat=0,
        bndout=0,
        sleout=0,
    ):
        """Obtain output files containing results information.

        Parameters
        ----------
        matsum : float
            Time interval between outputs of part energies.
        glstat : float
            Time interval between outputs of global statistics
            and energies.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        if matsum > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="MATSUM", dt=matsum, binary=1, lcur=0, ioopt=0))
        if glstat > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="GLSTAT", dt=glstat, binary=1, lcur=0, ioopt=0))
        if elout > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="ELOUT", dt=glstat, binary=1, lcur=0, ioopt=0))
        if nodout > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="NODOUT", dt=glstat, binary=1, lcur=0, ioopt=0))
        if rbdout > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="RBDOUT", dt=glstat, binary=1, lcur=0, ioopt=0))
        if rcforc > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="RCFORC", dt=glstat, binary=1, lcur=0, ioopt=0))
        if secforc > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="SECFORC", dt=glstat, binary=1, lcur=0, ioopt=0))
        if rwforc > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="RWFORC", dt=glstat, binary=1, lcur=0, ioopt=0))
        if abstat > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="ABSTAT", dt=glstat, binary=1, lcur=0, ioopt=0))
        if bndout > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="BNDOUT", dt=glstat, binary=1, lcur=0, ioopt=0))
        if nodfor > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="NODFOR", dt=glstat, binary=1, lcur=0, ioopt=0))
        if sleout > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="SLEOUT", dt=glstat, binary=1, lcur=0, ioopt=0))
        ret = 1
        logging.info("Output Setting...")
        return ret

    def save_file(self):
        """Save keyword files.

        Returns
        -------
        bool
            "True" when successful, "False" when failed
        """
        
        for obj in self.object_list:
            obj.save_file()

        ret = self.stub.SaveFile(SaveFileRequest(name=self.mainname))
        msg = self.mainname + " is outputed..."
        logging.info(msg)
        return ret