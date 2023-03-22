"""Module for launching kwserver locally."""

import os
import socket
import subprocess
import threading

from ansys.dyna.core.pre import LOG
from ansys.dyna.core.pre.misc import check_valid_ip, check_valid_port

LOCALHOST = "127.0.0.1"
DYNAPRE_DEFAULT_PORT = 50051


def check_ports(port_range, ip="localhost"):
    """Check the state of ports in a port range"""
    ports = {}
    for port in port_range:
        ports[port] = port_in_use(port, ip)
    return ports


def port_in_use(port, host=LOCALHOST):
    """Returns True when a port is in use at the given host.
    Must actually "bind" the address.  Just checking if we can create
    a socket is insufficient as it's possible to run into permission
    errors like:
    - An attempt was made to access a socket in a way forbidden by its
      access permissions.
    """
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        try:
            sock.bind((host, port))
            return False
        except:
            return True


def launch_grpc(port=DYNAPRE_DEFAULT_PORT, ip=LOCALHOST, server_path=None) -> tuple:  # pragma: no cover
    """Start kwserver locally in gRPC mode.
    Parameters
    ----------
    port : int
        Port to launch PyDyna gRPC on.  Final port will be the first
        port available after (or including) this port.

    Returns
    -------
    int
        Returns the port number that the gRPC instance started on.

    """
    LOG.debug("Starting 'launch_kwserver'.")

    command = "python kwserver.py"
    LOG.debug(f"Starting kwserver with command: {command}")

    # env_vars = update_env_vars(add_env_vars, replace_env_vars)
    LOG.info(f"Running in {ip}:{port} the following command: '{command}'")

    LOG.debug("kwserver starting in background.")
    process = subprocess.Popen(["python", "kwserver.py"], cwd=server_path, shell=True)
    process.wait()
    # while True:
    #     pass
    return port


class ServerThread(threading.Thread):
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
):
    """Start kwserver locally.
    Parameters
    ----------
    port : int
        Port to launch MAPDL gRPC on.
    ip : bool, optional
        Specify the IP address of the PyDyna instance to connect to.
        You can also provide a hostname as an alternative to an IP address.
        Defaults to ``'127.0.0.1'``. You can also override the
        default behavior of this keyword argument with the
    """

    check_valid_ip(ip)  # double check

    if port is None:
        port = int(os.environ.get("PYDYNAPRE_PORT", DYNAPRE_DEFAULT_PORT))
        check_valid_port(port)
        LOG.debug(f"Using default port {port}")

    if (ip.lower() == "localhost" or ip == LOCALHOST) and not DynaSolution.grpc_local_server_on():
        LOG.debug("Starting kwserver")
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
