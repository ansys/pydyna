"""Module for launching the pydyna solver service locally."""

import os
import platform
import socket
import subprocess
from time import sleep
from zipfile import ZipFile

from ansys.dyna.core.solver.dynalogging import LOG

try:
    import ansys.platform.instancemanagement as pypim

    _HAS_PIM = True
except ModuleNotFoundError:  # pragma: no cover
    _HAS_PIM = False

from ansys.dyna.core.solver import DynaSolver

LOCALHOST = "127.0.0.1"
DYNA_DEFAULT_PORT = 5000
SERVER_SOLVER_VERSION = "v0.4.12"
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


def launch_grpc(port=DYNA_DEFAULT_PORT, ip=LOCALHOST, server_path="") -> tuple:  # pragma: no cover
    """
    Launch the solver service locally in gRPC mode.

    Parameters
    ----------
    port : int, optional
        Port to launch the solver service on. The default is ``DYNAPRE_DEFAULT_PORT``.
        The final port is the first port available after (or including) this
        port.
    ip : str, optional
        IP address. The default is ``LOCALHOST``, in which case ``"127.0.0.1"``
        is used.
    server_path : string, optional
        Path to the solver service. The default is ``None``.

    Returns
    -------
    int
        Port number that the gRPC instance started on.
    """
    # start server locally
    if (ip.lower() == "localhost" or ip == LOCALHOST) and not DynaSolver.grpc_local_server_on():
        # LOG.debug("Starting solver server")

        if len(server_path) == 0:
            # server_path = os.getenv("ANSYS_PYDYNA_SOLVER_SERVER_PATH")
            # if server_path is None:
            url = "https://github.com/ansys/pydyna/releases/download/v0.4.12/ansys-pydyna-solver-server.zip"
            directory = DynaSolver.get_appdata_path()
            filename = directory + os.sep + "ansys-pydyna-solver-server.zip"
            server_package = directory + os.sep + "ansys-pydyna-solver-server"
            extractpath = directory
            if not os.path.exists(server_package):
                DynaSolver.downloadfile(url, filename)
                with ZipFile(filename, "r") as zipf:
                    zipf.extractall(extractpath)
            else:
                with ZipFile(filename, "r") as zipf:
                    zipinfo = zipf.getinfo("ansys-pydyna-solver-server/")
                    version = str(zipinfo.comment, encoding="utf-8")
                    if version != SERVER_SOLVER_VERSION:
                        DynaSolver.downloadfile(url, filename)
                        with ZipFile(filename, "r") as zipf:
                            zipf.extractall(extractpath)
            server_path = server_package
            # os.environ["ANSYS_PYDYNA_SOLVER_SERVER_PATH"] = server_path
        if os.path.isdir(server_path):
            # threadserver = ServerThread(1, port=port, ip=ip, server_path=server_path)
            # threadserver.run()
            # threadserver.setDaemon(True)
            # threadserver.start()
            if platform.system() == "Windows":
                process = subprocess.Popen("python server.py", cwd=server_path, shell=True)
            else:
                process = subprocess.Popen("python3 server.py", cwd=server_path, shell=True)
            waittime = 0
            while not DynaSolver.grpc_local_server_on():
                sleep(5)
                waittime += 5
                if waittime > 60:
                    print("Failed to start pydyna solver server locally")
                    break

        else:
            print("Failed to start pydyna solver server locally,Invalid server path!")

    LOG.debug("Starting 'launch_grpc'.")

    command = "python server.py"
    LOG.debug(f"Starting the solver service with command: {command}")

    # env_vars = update_env_vars(add_env_vars, replace_env_vars)
    LOG.info(f"Running in {ip}:{port} the following command: '{command}'")

    LOG.debug("the solver service starting in background.")
    # if platform.system() == "Windows":
    # process = subprocess.Popen("python server.py", cwd=server_path, shell=True)
    # else:
    # process = subprocess.Popen("python3 server.py", cwd=server_path, shell=True)
    # process.wait()
    # return port


def launch_remote_dyna(
    version=None,
    cleanup_on_exit=True,
) -> DynaSolver:
    """Start DYNA remotely using the product instance management API.

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
    ansys.dyna.core.solver.DynaSolver
        An instance of DynaSolver.
    """
    if not _HAS_PIM:  # pragma: no cover
        raise ModuleNotFoundError("The package 'ansys-platform-instancemanagement' is required to use this function.")

    pim = pypim.connect()
    instance = pim.create_instance(product_name="dynasolver", product_version=version)
    instance.wait_for_ready()
    channel = instance.build_grpc_channel(
        options=[
            ("grpc.max_receive_message_length", MAX_MESSAGE_LENGTH),
        ]
    )
    client = DynaSolver(channel=channel)
    client.pim_client = pim
    client.remote_instance = instance
    return client


def launch_dyna(
    port=None,
    ip=None,
) -> DynaSolver:
    """Start DYNA locally.

    Parameters
    ----------
    port : int
        Port to launch DYNA gRPC on.  Final port will be the first
        port available after (or including) this port.  Defaults to
        5000.

    ip : bool, optional
        You can provide a hostname as an alternative to an IP address.
        Defaults to ``'127.0.0.1'``.

    Examples
    --------
    Launch DYNA using the best protocol.

    >>> from ansys.dyna.core.solver import launch_dyna
    >>> solution = launch_dyna()


    Connect to an existing instance of DYNA at IP 192.168.1.30 and
    port 5000.

    >>> solution = launch_dyna(ip='192.168.1.30',port=5000)

    """

    # check_valid_ip(ip)  # double check

    # check_valid_port(port)

    # Start DYNA with PyPIM if the environment is configured for it
    # and the user did not pass a directive on how to launch it.
    if _HAS_PIM and pypim.is_configured():
        LOG.info("Starting DYNA remotely. The startup configuration will be ignored.")
        return launch_remote_dyna()

    launch_grpc(port=port, ip=ip)

    dyna = DynaSolver(
        hostname=ip,
        port=port,
    )

    return dyna


# class ServerThread(threading.Thread):
class ServerThread:
    """Provides server thread properties.

    Parameters
    ----------
    threadID :
    port :
    ip :
    server_path :

    """

    def __init__(self, threadID, port, ip, server_path):
        # threading.Thread.__init__(self)
        self.threadID = threadID
        self.port = port
        self.ip = ip
        self.server_path = server_path
        self.process = None

    def run(self):
        self.process = launch_grpc(ip=self.ip, port=self.port, server_path=self.server_path)

    def termination(self):
        self.process.termination()
