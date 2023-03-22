# -*- coding: utf-8 -*-
import logging
import os

import appdirs

from ansys.dyna.core.pre.dynalogging import Logger

LOG = Logger(level=logging.ERROR, to_file=False, to_stdout=True)
LOG.debug("Loaded logging module as LOG")

_LOCAL_PORTS = []

try:
    import importlib.metadata as importlib_metadata
except ModuleNotFoundError:  # pragma: no cover
    import importlib_metadata

# __version__ = importlib_metadata.version(__name__.replace(".", "-"))

from .dynabase import *
from .dynadem import DynaDEM
from .dynaicfd import DynaICFD
from .dynaiga import DynaIGA
from .dynamaterial import *
from .dynasale import *
