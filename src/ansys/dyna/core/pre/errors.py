"""pydyna specific errors"""

from functools import wraps

from grpc._channel import _InactiveRpcError, _MultiThreadedRendezvous

from ansys.dyna.core.pre import LOG as logger

SIGINT_TRACKER = []


LOCKFILE_MSG = """
Another ANSYS job with the same job name is already running in this
directory, or the lock file has not been deleted from an abnormally
terminated ANSYS run.

Disable this check by passing ``override=True``

"""


class VersionError(ValueError):
    """Raised when keserver is the wrong version"""

    def __init__(self, msg="Invalid kwserver version"):
        ValueError.__init__(self, msg)


class PydynaRuntimeError(RuntimeError):
    """Raised when PyDyna passes an error"""

    pass


class PydynaInvalidRoutineError(RuntimeError):
    """Raised when MAPDL is in the wrong routine"""

    def __init__(self, msg=""):
        RuntimeError.__init__(self, msg)


class LockFileException(RuntimeError):
    """Error message when the lockfile has not been removed"""

    def __init__(self, msg=LOCKFILE_MSG):
        RuntimeError.__init__(self, msg)


class KwserverDidNotStart(RuntimeError):
    """Error when the kwserver process does not start"""

    def __init__(self, msg=""):
        RuntimeError.__init__(self, msg)

