"""Module for launching the pre service locally."""

import os
import socket
import subprocess

from ansys.dyna.core.pre import LOG
from ansys.dyna.core.pre.misc import check_valid_ip, check_valid_port

LOCALHOST = "127.0.0.1"
DYNAPRE_DEFAULT_PORT = 50051


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


def launch_grpc(port=DYNAPRE_DEFAULT_PORT, ip=LOCALHOST, server_path=None) -> tuple:  # pragma: no cover
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
    LOG.debug("Starting 'launch_grpc'.")

    command = "python kwserver.py"
    LOG.debug(f"Starting the pre service with command: {command}")

    # env_vars = update_env_vars(add_env_vars, replace_env_vars)
    LOG.info(f"Running in {ip}:{port} the following command: '{command}'")

    LOG.debug("the pre service starting in background.")
    process = subprocess.Popen("python kwserver.py", cwd=server_path, shell=True)
    # process.wait()
    # while True:
    #     pass
    return port


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


def launch_dynapre(
    port=50051,
    ip="localhost",
):
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

    if (ip.lower() == "localhost" or ip == LOCALHOST) and not DynaSolution.grpc_local_server_on():
        LOG.debug("Starting the pre service")
        server_path = os.path.join(os.getcwd(), "../../src/ansys/dyna/core/pre/Server")
        threadserver = ServerThread(1, port=port, ip=ip, server_path=server_path)
        threadserver.setDaemon(True)
        threadserver.start()

    # dynasln = DynaSolution(
    # hostname = ip,
    # port=port,
    # )

    # return dynasln


if __name__ == "__main__":
    server_path = os.path.join(os.getcwd(), "Server")
    process = subprocess.Popen(["python", "kwserver.py"], cwd=server_path, shell=True)
    process.wait()
    process.terminate()
    print(process)
