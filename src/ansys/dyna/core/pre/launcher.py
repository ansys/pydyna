"""Module for launching the pre service locally."""

import os
import socket
import subprocess
import sys
import threading
from time import sleep
from zipfile import ZipFile

from ansys.dyna.core.pre.dynalogging import LOG
from ansys.dyna.core.pre.misc import check_valid_ip, check_valid_port

try:
    import ansys.platform.instancemanagement as pypim

    _HAS_PIM = True
except ModuleNotFoundError:  # pragma: no cover
    _HAS_PIM = False

from ansys.dyna.core.pre.dynasolution import DynaSolution

LOCALHOST = "127.0.0.1"
DYNAPRE_DEFAULT_PORT = 50051
SERVER_PRE_VERSION = "v0.4.6"
MAX_MESSAGE_LENGTH = 8 * 1024**2


def check_ports(port_range, ip="localhost"):
    """Check the state of ports in a port range.

    Parameters
    ----------
    port_range :
    ip : str, optional
        IP address. The default is ``"localhost"``, in which case
        ``"127.0.0.1"``is used.
    """
    ports = {}
    for port in port_range:
        ports[port] = port_in_use(port, ip)
    return ports


def port_in_use(port, host=LOCALHOST):
    """
    Determine if a port is in use at a given host.

    Parameters
    ----------
    port : int
       Port.
    host :
       Host. The default is ``LOCALHOST``, in which case ``"127.0.0.1"``
        is used.

    Returns
    -------
    ``True`` when a port is in use at the given host, ``False`` otherwise.

    Notes
    -----
    The port must "bind" the address. Just checking if a socket can be created
    is insufficient because it is possible to run into permission
    errors like this one:

    "An attempt was made to access a socket in a way forbidden by its
    access permissions."
    """
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        try:
            sock.bind((host, port))
            return False
        except:
            return True


def get_virtualenv_path():
    """Get the virtual environment path."""

    python_executable_path = sys.executable
    if hasattr(sys, "real_prefix"):
        virtual_env_path = sys.prefix
    else:
        virtual_env_path = os.path.dirname(python_executable_path)
    return virtual_env_path


def launch_grpc(port=DYNAPRE_DEFAULT_PORT, ip=LOCALHOST, server_path="") -> tuple:  # pragma: no cover
    """
    Launch the pre service locally in gRPC mode.

    Parameters
    ----------
    port : int, optional
        Port to launch the pre service on. The default is ``DYNAPRE_DEFAULT_PORT``.
        The final port is the first port available after (or including) this
        port.
    ip : str, optional
        IP address. The default is ``LOCALHOST``, in which case ``"127.0.0.1"``
        is used.
    server_path : string, optional
        Path to the pre service. The default is ``None``.

    Returns
    -------
    int
        Port number that the gRPC instance started on.
    """
    # start server locally
    if (ip.lower() == "localhost" or ip == LOCALHOST) and not DynaSolution.grpc_local_server_on():
        LOG.debug("Starting kwserver")
        # download server form webset
        if len(server_path) == 0:
            url = "https://github.com/ansys/pydyna/releases/download/v0.4.6/ansys-pydyna-pre-server.zip"
            directory = DynaSolution.get_appdata_path()
            filename = directory + os.sep + "ansys-pydyna-pre-server.zip"
            server_package = directory + os.sep + "ansys-pydyna-pre-server"
            extractpath = directory
            if not os.path.exists(server_package):
                DynaSolution.downloadfile(url, filename)
                with ZipFile(filename, "r") as zipf:
                    zipf.extractall(extractpath)
            else:
                with ZipFile(filename, "r") as zipf:
                    zipinfo = zipf.getinfo("ansys-pydyna-pre-server/")
                    version = str(zipinfo.comment, encoding="utf-8")
                    if version != SERVER_PRE_VERSION:
                        DynaSolution.downloadfile(url, filename)
                        with ZipFile(filename, "r") as zipf:
                            zipf.extractall(extractpath)
            server_path = server_package
        if os.path.isdir(server_path):
            # threadserver = ServerThread(1, port=port, ip=hostname, server_path=server_path)
            # threadserver.run()
            # threadserver.setDaemon(True)
            # threadserver.start()
            # env_path = get_virtualenv_path()
            process = subprocess.Popen("python kwserver.py", cwd=server_path, shell=True)
            waittime = 0
            while not DynaSolution.grpc_local_server_on():
                sleep(5)
                waittime += 5
                if waittime > 60:
                    print("Failed to start pydyna pre server locally")
                    break
        else:
            print("Failed to start pydyna pre server locally,Invalid server path!")

    LOG.debug("Starting 'launch_grpc'.")

    command = "python kwserver.py"
    LOG.debug(f"Starting the pre service with command: {command}")

    # env_vars = update_env_vars(add_env_vars, replace_env_vars)
    LOG.info(f"Running in {ip}:{port} the following command: '{command}'")

    LOG.debug("the pre service starting in background.")
    # process = subprocess.Popen("python kwserver.py", cwd=server_path, shell=True)
    # process.wait()
    # return port


def launch_remote_dynapre(
    version=None,
    cleanup_on_exit=True,
) -> DynaSolution:
    """Start DYNA PRE remotely using the product instance management API.

    When calling this method, you need to ensure that you are in an environment where PyPIM is configured.
    This can be verified with :func:`pypim.is_configured <ansys.platform.instancemanagement.is_configured>`.

    Parameters
    ----------
    version : str, optional
        The DYNA version to run, in the 3 digits format, such as "212".

        If unspecified, the version will be chosen by the server.

    cleanup_on_exit : bool, optional
        Exit DYNA when python exits or the dyna Python instance is
        garbage collected.

        If unspecified, it will be cleaned up.

    Returns
    -------
    ansys.dyna.core.pre.DynaSolution
        An instance of DynaSolution.
    """
    if not _HAS_PIM:  # pragma: no cover
        raise ModuleNotFoundError("The package 'ansys-platform-instancemanagement' is required to use this function.")

    pim = pypim.connect()
    instance = pim.create_instance(product_name="dynapre", product_version=version)
    instance.wait_for_ready()
    channel = instance.build_grpc_channel(
        options=[
            ("grpc.max_receive_message_length", MAX_MESSAGE_LENGTH),
        ]
    )
    client = DynaSolution(channel=channel)
    client.pim_client = pim
    client.remote_instance = instance
    return client


class ServerThread(threading.Thread):
    # class ServerThread:
    """Provides server thread properties.

    Parameters
    ----------
    threadID :
    port :
    ip :
    server_path :


    """

    def __init__(self, threadID, port, ip, server_path):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.port = port
        self.ip = ip
        self.server_path = server_path
        self.process = None

    def run(self):
        self.process = launch_grpc(ip=self.ip, port=self.port, server_path=self.server_path)

    def termination(self):
        self.process.termination()


def launch_dynapre(
    port=50051,
    ip="localhost",
) -> DynaSolution:
    """
    Start the pre service locally.

    Parameters
    ----------
    port : int, optional
        Port to launch MAPDL gRPC on. The default is ``50051``.
    ip : bool, optional
        IP address or host name of the PyDYNA instance to connect to.
        The default is ``"localhost"``, in which case ``"127.0.0.1"``
        is used.

    """

    check_valid_ip(ip)  # double check

    if port is None:
        port = int(os.environ.get("PYDYNAPRE_PORT", DYNAPRE_DEFAULT_PORT))
        check_valid_port(port)
        LOG.debug(f"Using default port {port}")

    # Start DYNA Pre with PyPIM if the environment is configured for it
    # and the user did not pass a directive on how to launch it.
    if _HAS_PIM and pypim.is_configured():
        LOG.info("Starting DYNA Pre remotely. The startup configuration will be ignored.")
        return launch_remote_dynapre()

    launch_grpc(port=port, ip=ip)

    dynapre = DynaSolution(
        hostname=ip,
        port=port,
    )

    return dynapre

    if (ip.lower() == "localhost" or ip == LOCALHOST) and not DynaSolution.grpc_local_server_on():
        LOG.debug("Starting the pre service")
        server_path = os.path.join(os.getcwd(), "../../src/ansys/dyna/core/pre/Server")
        threadserver = ServerThread(1, port=port, ip=ip, server_path=server_path)
        threadserver.setDaemon(True)
        threadserver.start()


if __name__ == "__main__":
    server_path = os.path.join(os.getcwd(), "Server")
    process = subprocess.Popen(["python", "kwserver.py"], cwd=server_path, shell=True)
    process.wait()
    process.terminate()
    print(process)
