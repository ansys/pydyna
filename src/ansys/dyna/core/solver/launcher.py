"""Module for launching the pydyna solver service locally."""

import socket
import subprocess
import threading

from ansys.dyna.core.pre import LOG

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
    LOG.debug("Starting 'launch_grpc'.")

    command = "python kwserver.py"
    LOG.debug(f"Starting the solver service with command: {command}")

    # env_vars = update_env_vars(add_env_vars, replace_env_vars)
    LOG.info(f"Running in {ip}:{port} the following command: '{command}'")

    LOG.debug("the solver service starting in background.")
    process = subprocess.Popen("python server.py", cwd=server_path, shell=True)
    process.wait()
    return port


class ServerThread(threading.Thread):
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
