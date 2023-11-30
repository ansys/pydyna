"""
Solution
========

Module for providing the top object that sets up a DYNA solution.
"""

import logging
import os
import sys

from ansys.api.dyna.v0.kwprocess_pb2 import *  # noqa : F403
from ansys.api.dyna.v0.kwprocess_pb2_grpc import *  # noqa : F403

# from subprocess import DETACHED_PROCESS
import grpc

from .launcher import *  # noqa : F403

CHUNK_SIZE = 1024 * 1024


def init_log(log_file):
    """Initialize a log file.

    Parameters
    ----------
    log_file : str
        Name of the log file.
    """
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
    """Contains methods for creating a general LS-DYNA keyword.

    Parameters
    ----------
    hostname : str, optional
       Host name. The default is ``"localhost"``.
    port : str, optional
       Port. the default is ``"50051"``.
    """

    def __init__(self, hostname="localhost", port="50051", server_path=""):
        # launch server
        check_valid_ip(hostname)  # double check
        if port is None:
            port = int(os.environ.get("PYDYNAPRE_PORT", DYNAPRE_DEFAULT_PORT))
            check_valid_port(port)
            LOG.debug(f"Using default port {port}")

        # start server locally
        if (hostname.lower() == "localhost" or hostname == LOCALHOST) and not DynaSolution.grpc_local_server_on():
            LOG.debug("Starting kwserver")

            if len(server_path) == 0:
                server_path = os.getenv("ANSYS_PYDYNA_PRE_SERVER_PATH")
                if server_path is None:
                    print("Please set the environment variable for ANSYS_PYDYNA_PRE_SERVER_PATH")
                    return False
            if os.path.isdir(server_path):
                threadserver = ServerThread(1, port=port, ip=hostname, server_path=server_path)
                threadserver.setDaemon(True)
                threadserver.start()
            else:
                print("Failed to start pydyna pre server locally,Invalid server path!")

        init_log("client.log")
        temp = hostname + ":" + str(port)
        channel = grpc.insecure_channel(temp)
        try:
            grpc.channel_ready_future(channel).result(timeout=5)
        except grpc.FutureTimeoutError:
            logging.critical("Can not connect to kwServer")
            sys.exit()
        logging.info("Connected to kwServer...")
        self.stub = kwC2SStub(channel)
        self.object_list = []
        self.mainname = ""
        self._path = None
        DynaSolution.stub = self.stub
        DynaSolution.terminationtime = 0

    @staticmethod
    def grpc_local_server_on() -> bool:
        """Check if the server is launched locally.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        channel = grpc.insecure_channel("localhost:50051")
        try:
            grpc.channel_ready_future(channel).result(timeout=5)
        except:
            return False
        return True

    def get_stub():
        """Get the stub of the solution object."""
        return DynaSolution.stub

    def add(self, obj):
        """Add a case in the solution.

        Parameters
        ----------
        obj :

        """
        self.object_list.append(obj)

    def get_file_chunks(self, filename):
        """Get file chunks.

        Parameters
        ----------
        filename : str
            Name of the file.
        """
        with open(filename, "rb") as f:
            while True:
                piece = f.read(CHUNK_SIZE)
                if len(piece) == 0:
                    return
                yield Chunk(buffer=piece)

    def upload(self, stub_, filename):
        """Upload files to the server.

        Parameters
        ----------
        stub_ :
        filename : str
            Name of the file.

        """
        chunks_generator = self.get_file_chunks(filename)
        response = stub_.Upload(chunks_generator)

    def download(self, remote_name, local_name):
        """Download files from the server.

        Parameters
        ----------
        stub_ :
        remote_name :
        local_name :

        """
        response = self.stub.Download(DownloadRequest(url=remote_name))
        with open(local_name, "wb") as f:
            for chunk in response:
                f.write(chunk.buffer)

    def open_files(self, filenames):
        """Open initial model files.

        Parameters
        ----------
        filenames : list
            List of filenames. The main file is ``[0]``. The others are subfiles.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        splitfiles = os.path.split(filenames[0])
        path = splitfiles[0]
        for filename in filenames:
            fn = os.path.basename(filename)
            self.stub.kwSetFileName(kwFileName(name=fn, num=filenames.index(filename)))
            self.upload(self.stub, path + os.sep + fn)
            logging.info(path + os.sep + fn + " uploaded to server...")

        self.mainname = os.path.basename(filenames[0])
        return self.stub.LoadFile(LoadFileRequest())

    def set_termination(self, termination_time):
        """Set time for terminating the job.

        Parameters
        ----------
        termination_time : float
            Termination time.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
        """
        DynaSolution.termination_time = termination_time
        ret = self.stub.CreateTermination(TerminationRequest(endtim=termination_time))
        logging.info("Setting termination time ...")
        return ret

    def create_database_binary(self, filetype="D3PLOT", dt=0, maxint=3, ieverp=0, dcomp=1, nintsld=1):
        """Request binary output.

        Parameters
        ----------
        filetype : str, optional
           Type of file. The default is ``"D3PLOT"``.
        dt : float, optional
            Time interval between output states. The default is ``0``.
        maxint : int, optional
            Number of shell and thick shell through-thickness integration points
            to output to the d3plot. The default is ``3``.
        ieverp : int, optional
            How to plot output states on plot files. The default is ``0``. Every output
            state for the d3plot database is written to a separate file. Options are:

            - EQ.0: More than one state can be on each plot file.
            - EQ.1: Only one state can be on each plot file.

        dcomp : int, optional
            Data compression to eliminate rigid body data. The default is ``1``.
        nintsld : int, optional
            Number of solid element integration points written to the LS-DYNA database.
            The default is ``1``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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
        """Obtain output files containing result information.

        Parameters
        ----------
        type : string
            Type of the database. Options are:

            - BNDOUT
            - GLSTAT
            - MATSUM
            - NODFOR
            - RCFORC
            - SLEOUT

        dt : float, optional
            Time interval between outputs. The default is ``0.0``.
        binary : int, optional
            Flag for whether to generate binary output. The default is ``1``.
        lcur : int, optional
            Curve ID specifying the time interval between outputs. The default
            is ``0``.
        ioopt : int, optional
            Flag for governing the behavior of the output frequency load curve
            defined by the ``lcur`` parameter. The default is ``0``.

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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
        sphmassflow=0,
    ):
        """Obtain output files containing result information.

        Parameters
        ----------
        matsum : float, optional
            Time interval between outputs of part energies. The default is ``0``.
        glstat : float, optional
            Time interval between outputs of global statistics
            and energies. The default is ``0``.
        elout :
        nodout :
        modfor :
        rbdout :
        secforc :
        rwforce :
        bndout :
        sleout :
        sphmassflow :

        Returns
        -------
        bool
            ``True`` when successful, ``False`` when failed.
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
        if sphmassflow > 0:
            self.stub.CreateDBAscii(DBAsciiRequest(type="SPHMASSFLOW", dt=glstat, binary=1, lcur=0, ioopt=0))
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

        for obj in self.object_list:
            obj.save_file()

        ret = self.stub.SaveFile(SaveFileRequest(name=self.mainname))
        msg = self.mainname + " is outputed..."
        logging.info(msg)
        return ret.outpath
