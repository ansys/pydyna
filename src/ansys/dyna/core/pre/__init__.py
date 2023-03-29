# -*- coding: utf-8 -*-
import logging
import os

from ansys.dyna.core.pre.dynalogging import Logger

LOG = Logger(level=logging.ERROR, to_file=False, to_stdout=True)
LOG.debug("Loaded logging module as LOG")

_LOCAL_PORTS = []

from .dynabase import *
from .dynadem import DynaDEM
from .dynaicfd import DynaICFD
from .dynaiga import DynaIGA
from .dynamaterial import *
from .dynasale import *
