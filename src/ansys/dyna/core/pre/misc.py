"""Module providing miscellaneous functions and methods."""
import inspect
import os
import random
import socket
import string
import sys
import tempfile

# path of this module
MODULE_PATH = os.path.dirname(inspect.getfile(inspect.currentframe()))


class Plain_Report:
    def __init__(self, core, optional=None, additional=None, **kwargs):
        """
        Provides the base class for a plain report.

        This class is based on the `scooby <https://github.com/banesullivan/scooby>`_
        package.

        Parameters
        ----------
        core : iter[str]
            Core packages to list first.
        optional : iter[str], optional
            List of packages to list if they are available. The default is ``None``.
            If no packages are available, no warnings or errors are raised.
        additional : iter[str], optional
            List of packages or package names to add to the output information.
            The default is ``None``.
        **kwargs : dict
            Dictionary of keyword arguments.
        """

        self.additional = additional
        self.core = core
        self.optional = optional
        self.kwargs = kwargs

        if os.name == "posix":
            self.core.extend(["pexpect"])

        if self.optional is not None and sys.version_info[1] < 9:
            self.optional.append("ansys_corba")

        # Information about the GPU - bare except in case there is a rendering
        # bug that the user is trying to report.
        if self.kwargs.get("gpu", False) and _HAS_PYVISTA:
            from pyvista.utilities.errors import GPUInfo

            try:
                self.kwargs["extra_meta"] = [(t[1], t[0]) for t in GPUInfo().get_info()]
            except RuntimeError as e:  # pragma: no cover
                self.kwargs["extra_meta"] = ("GPU Details", f"Error: {str(e)}")
        else:
            self.kwargs["extra_meta"] = ("GPU Details", "None")

    def get_version(self, package):
        try:
            import importlib.metadata as importlib_metadata
        except ModuleNotFoundError:  # pragma: no cover
            import importlib_metadata

        try:
            return importlib_metadata.version(package.replace(".", "-"))
        except importlib_metadata.PackageNotFoundError:
            return "Package not found"

    def __repr__(self):
        header = [
            "-" * 79,
            "\n",
            "PyMAPDL Software and Environment Report",
            "\n",
            "Packages Requirements",
            "*********************",
        ]

        core = ["\nCore packages", "-------------"]
        core.extend([f"{each.ljust(20)}: {self.get_version(each)}" for each in self.core if self.get_version(each)])

        if self.optional:
            optional = ["\nOptional packages", "-----------------"]
            optional.extend(
                [f"{each.ljust(20)}: {self.get_version(each)}" for each in self.optional if self.get_version(each)]
            )
        else:
            optional = [""]

        if self.additional:
            additional = ["\nAdditional packages", "-----------------"]
            additional.extend(
                [f"{each.ljust(20)}: {self.get_version(each)}" for each in self.additional if self.get_version(each)]
            )
        else:
            additional = [""]

        return "\n".join(header + core + optional + additional) + self.mapdl_info()


# Determine which type of report will be used (depending on the
# available packages)
base_report_class = Plain_Report


def is_float(input_string):
    """Determine if a string can be converted to a float.

    Parameters
    ----------
    input_string : str
        String.

    Returns
    -------
        ``True`` when the string can be converted to a float, ``False`` otherwise."""
    try:
        float(input_string)
        return True
    except ValueError:
        return False


def random_string(stringLength=10, letters=string.ascii_lowercase):
    """Generate a random string of a fixed length.

    Parameters
    ----------
    stringLength : int, optional
        Length of the string. The default is ``10``.
    letters :

    """
    return "".join(random.choice(letters) for i in range(stringLength))


def create_temp_dir(tmpdir=None):
    """Create a unique working directory in a temporary directory.

    Parameters
    ----------
    tempdir : str, optional
       Name of the temporary directory to create the working
       directory in. The default is ``None``.

    """
    if tmpdir is None:
        tmpdir = tempfile.gettempdir()
    elif not os.path.isdir(tmpdir):
        os.makedirs(tmpdir)

    # running into a rare issue with MAPDL on Windows with "\n" being
    # treated literally.
    letters = string.ascii_lowercase.replace("n", "")
    path = os.path.join(tmpdir, random_string(10, letters))

    # in the *rare* case of a duplicate path
    while os.path.isdir(path):
        path = os.path.join(tempfile.gettempdir(), random_string(10, letters))

    try:
        os.mkdir(path)
    except:
        raise RuntimeError(
            "Unable to create temporary working " "directory %s\n" % path + "Please specify run_location="
        )

    return path


def check_valid_ip(ip):
    """Check if an IP address is valid.

    Parameters
    ----------
    ip :
        IP address.
    """
    if ip.lower() == "localhost":
        return True
    ip = ip.replace('"', "").replace("'", "")
    try:
        socket.inet_aton(ip)
        return True
    except socket.error:
        return False


def check_valid_port(port, lower_bound=1000, high_bound=60000):
    """Check if a port is valid.

    Parameters
    ----------
    port : int
        Port.
    lower_bound : int, optional
        Lowest value for the port range. The default is ``1000``.
    high_bound : int, optional
        Highest value for the port range. The default is ``6000``.
    """
    if not isinstance(port, int):
        raise ValueError("The 'port' parameter should be an integer.")

    if lower_bound < port < high_bound:
        return
    else:
        raise ValueError(f"'port' values should be between {lower_bound} and {high_bound}.")
