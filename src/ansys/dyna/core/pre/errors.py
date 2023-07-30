"""Module containing PyDYNA-specific errors."""


SIGINT_TRACKER = []


LOCKFILE_MSG = """
Either another Ansys job with the same job name is already running in this
directory or the lock file has not been deleted from an abnormally
terminated Ansys run.

Disable this check by passing ``override=True``

"""


class VersionError(ValueError):
    """Provides the error raised when the kwserver is invalid."""

    def __init__(self, msg="Invalid kwserver version"):
        ValueError.__init__(self, msg)


class PydynaRuntimeError(RuntimeError):
    """Provides the error raised when PyDYNA passes an error."""

    pass


class PydynaInvalidRoutineError(RuntimeError):
    """Provides the error raised when MAPDL is in the wrong routine."""

    def __init__(self, msg=""):
        RuntimeError.__init__(self, msg)


class LockFileException(RuntimeError):
    """Provides the error raised when the lockfile has not been removed."""

    def __init__(self, msg=LOCKFILE_MSG):
        RuntimeError.__init__(self, msg)


class KwserverDidNotStart(RuntimeError):
    """Provides the error raised when the kwserver process does not start."""

    def __init__(self, msg=""):
        RuntimeError.__init__(self, msg)
