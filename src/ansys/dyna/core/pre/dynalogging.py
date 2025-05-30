# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""Logging module supplying a general framework for logging in the PyDYNA
``pre`` service.

This module is built upon the `Python logging <https://docs.python.org/3/library/logging.html>`_
module. It is not intended to replace this Python module but rather to provide a way
for it and the PyDyna ``pre`` service to interact.

The loggers used in this PyDyna logging mdoule include the name of the instance,
which is intended to be unique. This name is printed in all the active
outputs and is used to track the different instances of the PyDyNA ``pre``
service.

Global logger
-------------
The global logger, named ``pydyna_global``, is created at
``ansys.dyna.core.__init__``.  If you want to use the global logger,
you must call it at the top of your module:

.. code:: python

   from ansys.dyna.core.pre import LOG

You can rename the global logger to avoid conflicts with other loggers (if any):

.. code:: python

   from ansys.dyna.core.pre import LOG as logger


The default logging level of ``LOG`` is ``ERROR``. To change this to output
lower-level messages, you can use this code:

.. code:: python

   LOG.logger.setLevel("DEBUG")
   LOG.file_handler.setLevel("DEBUG")  # If present.
   LOG.stdout_handler.setLevel("DEBUG")  # If present.


Alternatively, you can set the logging level of ``LOG`` to ``DEBUG``
with one line of code::

.. code:: python

   LOG.setLevel("DEBUG")

Using the preceding line ensures that all the handlers are set to the input log level.

By default, this logger does not log to a file. If you want to log to a file,
you can add a file handler:

.. code:: python

   import os

   file_path = os.path.join(os.getcwd(), "pydyna.log")
   LOG.log_to_file(file_path)

The preceding code sets the logger to also redirect to a file. If you want
to change the characteristics of the global logger from the beginning
of the execution, you must edit the ``__init__`` file in the
``ansys.dyna.core.pre`` directory.

To log using this logger, call the desired method as a normal logger.


Instance loggers
----------------
Every time an instance of the :class:`Mapdl <ansys.mapdl.core.mapdl._MapdlCore>`
class is created, a logger is created and stored in two places:

* ``_MapdlCore._log``: For backward compatibility.
* ``LOG._instances``: This field is a dictionary  where the key is the name
  of the created logger.

Instance loggers inheritate the ``pydyna_global`` output handlers and
logging level unless otherwise specified. Instance loggers work in a
similar way to the global logger. You can use the
:func:`log_to_file() <PymapdlCustomAdapter.log_to_file>` method to add
a file handler or the :func:`logger.Logging.setLevel` method to change
the log level.

Other loggers
~~~~~~~~~~~~~
You can create your own loggers using the Python ``logging`` module as
you would do in any other script. No conflicts between these loggers exist.

"""

from copy import copy
from datetime import datetime
import logging
import sys
import weakref

## Default configuration
LOG_LEVEL = logging.DEBUG
FILE_NAME = "pydyna.log"

# For convenience
DEBUG = logging.DEBUG
INFO = logging.INFO
WARN = logging.WARN
ERROR = logging.ERROR
CRITICAL = logging.CRITICAL

## Formatting

STDOUT_MSG_FORMAT = "%(levelname)s - %(instance_name)s -  %(module)s - %(funcName)s - %(message)s"

FILE_MSG_FORMAT = STDOUT_MSG_FORMAT

DEFAULT_STDOUT_HEADER = """
LEVEL - INSTANCE NAME - MODULE - FUNCTION - MESSAGE
"""
DEFAULT_FILE_HEADER = DEFAULT_STDOUT_HEADER

NEW_SESSION_HEADER = f"""
===============================================================================
       NEW SESSION - {datetime.now().strftime("%m/%d/%Y, %H:%M:%S")}
==============================================================================="""

string_to_loglevel = {
    "DEBUG": DEBUG,
    "INFO": INFO,
    "WARN": WARN,
    "WARNING": WARN,
    "ERROR": ERROR,
    "CRITICAL": CRITICAL,
}


class PymapdlCustomAdapter(logging.LoggerAdapter):
    """Adapter for keeping the reference to an MAPDL instance name dynamic.

    Using the standard approach, extra parameters must be supplied
    to the logger to indicate the MAPDL instance for which messages
    must be logged.

    With this class, you only have to specify the MAPDL instance
    that you are referring to once.
    """

    level = None  # This is maintained for compatibility with the ``supress_logging`` method, but it does nothing.
    file_handler = None
    stdout_handler = None

    def __init__(self, logger, extra=None):
        self.logger = logger
        if extra is not None:
            self.extra = weakref.proxy(extra)
        else:
            self.extra = None
        self.file_handler = logger.file_handler
        self.std_out_handler = logger.std_out_handler

    def process(self, msg, kwargs):
        kwargs["extra"] = {}
        # These are the extra parameters sent to the log
        kwargs["extra"]["instance_name"] = self.extra.name  # here self.extra is the argument passed to the log records.
        return msg, kwargs

    def log_to_file(self, filename=FILE_NAME, level=LOG_LEVEL):
        """Add a file handler to the logger.

        Parameters
        ----------
        filename : str, optional
            Name of the file where logs are recorded. The default is ``FILE_NAME``.
        level : str, optional
            Level of logging. The default is ``LOG_LEVEL``, which causes all messages
            to be recorded. For example, you can set the level of logging to ``DEBUG``.
        """

        self.logger = addfile_handler(self.logger, filename=filename, level=level, write_headers=True)
        self.file_handler = self.logger.file_handler

    def log_to_stdout(self, level=LOG_LEVEL):
        """Add a standard output handler to the logger.

        Parameters
        ----------
        level : str, optional
            Level of logging record. The default is ``LOG_LEVEL``, which causes all messages
            to be recorded. For example, you can set the level of logging to ``"DEBUG"``.
        """
        if self.std_out_handler:
            raise Exception("Stdout logger already defined.")

        self.logger = add_stdout_handler(self.logger, level=level)
        self.std_out_handler = self.logger.std_out_handler

    def setLevel(self, level="DEBUG"):
        """Change the log level of the object and the attached handlers.

        Parameters
        ----------
        level : str, optional
            Level of logging record. The default is ``"DEBUG``.

        """
        self.logger.setLevel(level)
        for each_handler in self.logger.handlers:
            each_handler.setLevel(level)
        self.level = level


class PymapdlPercentStyle(logging.PercentStyle):
    def __init__(self, fmt, *, defaults=None):
        self._fmt = fmt or self.default_format
        self._defaults = defaults

    def _format(self, record):
        defaults = self._defaults
        if defaults:
            values = defaults | record.__dict__
        else:
            values = record.__dict__

        # You can do here any changes that you want in record, such as adding a key.

        # You could create an if here if you want conditional formatting, and even
        # change the record.__dict__.
        # Since we don't want to create conditional fields currently, it is fine to keep
        # the same MSG_FORMAT for all of them.

        # For the case of logging exceptions to the logger.
        values.setdefault("instance_name", "")

        return STDOUT_MSG_FORMAT % values


class PymapdlFormatter(logging.Formatter):
    """Provides a customized ``Formatter`` class for overwriting the default format styles."""

    def __init__(
        self,
        fmt=STDOUT_MSG_FORMAT,
        datefmt=None,
        style="%",
        validate=True,
        defaults=None,
    ):
        super().__init__(fmt, datefmt, style, validate)
        self._style = PymapdlPercentStyle(fmt, defaults=defaults)  # overwriting


class InstanceFilter(logging.Filter):
    """Ensures that the ``instance_name`` record always exists."""

    def filter(self, record):
        if not hasattr(record, "instance_name"):
            record.instance_name = ""
        return True


class Logger:
    """Provides the logger used for each PyDyna ``pre`` session.

    This class allows you to add handlers to the logger to output to a file or
    standard output.

    Parameters
    ----------
    level : int, optional
        Logging level to filter the message severity allowed in the logger.
        The default is ``logging.DEBUG``.
    to_file : bool, optional
        Whether to write log messages to a file. The default is ``False``.
    to_stdout : bool, optional
        Whether to write log messages to the standard output. The default is
        ``True``.
    filename : str, optional
        Name of the file to write log messages to if ``to_file=True``. The default
        is ``FILE_NAME``.

    Examples
    --------
    Demonstrate logger usage from an MAPDL instance mapdl. This logger is
    automatically created when an MAPDL instance is created.

    >>> from ansys.mapdl.core import launch_mapdl
    >>> mapdl = launch_mapdl(loglevel='DEBUG')
    >>> mapdl._log.info('This is a useful message')
    INFO -  -  <ipython-input-24-80df150fe31f> - <module> - This is LOG debug message.

    Import the global PYMAPDL logger and add a file output handler.

    >>> import os
    >>> from ansys.mapdl.core import LOG
    >>> file_path = os.path.join(os.getcwd(), 'pymapdl.log')
    >>> LOG.log_to_file(file_path)

    """

    file_handler = None
    std_out_handler = None
    _level = logging.DEBUG
    _instances = {}

    def __init__(self, level=logging.DEBUG, to_file=False, to_stdout=True, filename=FILE_NAME):
        """Customized the logger for the PyDYNA ``pre`` service.

        Parameters
        ----------
        level : str, optional
            Level of logging as defined in the ``logging`` package. The default is ``DEBUG``.
        to_file : bool, optional
            Whether to record the logs in a file. The default is ``False``.
        to_stdout : bool, optional
            Whether to output the logs to the standard output, which is the
            command line. The default is ``True``.
        filename : str, optional
            Name of the file where the logs are recorded if ``to_file=True``. The default
            is ``FILE_NAME``, in which case they are recorded in the
            ``'pymapdl.log'`` file.
        """

        # create default main logger
        self.logger = logging.getLogger("pydyna_global")
        self.logger.addFilter(InstanceFilter())
        self.logger.setLevel(level)
        self.logger.propagate = True
        self.level = self.logger.level  # TODO: TO REMOVE

        # Writing logging methods.
        self.debug = self.logger.debug
        self.info = self.logger.info
        self.warning = self.logger.warning
        self.error = self.logger.error
        self.critical = self.logger.critical
        self.log = self.logger.log

        if to_file or filename != FILE_NAME:
            # We record to file
            self.log_to_file(filename=filename, level=level)

        if to_stdout:
            self.log_to_stdout(level=level)

        # Using logger to record unhandled exceptions
        self.add_handling_uncaught_expections(self.logger)

    def log_to_file(self, filename=FILE_NAME, level=LOG_LEVEL):
        """Add a file handler to logger.

        Parameters
        ----------
        filename : str, optional
            Name of the file where the logs are recorded. The default
            is ``FILE_NAME``, in which case they are recorded in the
            ``'pymapdl.log'`` file.
        level : str, optional
            Level of logging. The default is ``LOG_LEVEL``, in which
            case ``'DEBUG'`` is used.

        Examples
        --------
        Write to the ``pymapdl.log`` file in the current working directory.

        >>> from ansys.mapdl.core import LOG
        >>> import os
        >>> file_path = os.path.join(os.getcwd(), 'pymapdl.log')
        >>> LOG.log_to_file(file_path)

        """

        self = addfile_handler(self, filename=filename, level=level, write_headers=True)

    def log_to_stdout(self, level=LOG_LEVEL):
        """Add a standard output handler to the logger.

        Parameters
        ----------
        filename : str, optional
            Name of the file where the logs are recorded. The default
            is ``FILE_NAME``, in which case they are recorded in the
            ``'pymapdl.log'`` file.
        level : str, optional
            Level of logging. The default is ``LOG_LEVEL``, in which
            case ``'DEBUG'`` is used.
        write_headers : bool, optional
            Whether to write the headers to the file. The default is ``True``.
        """

        self = add_stdout_handler(self, level=level)

    def setLevel(self, level="DEBUG"):
        """Change the log level of the object and the attached handlers.

        Parameters
        ----------
        level : str, optional
            Level of logging. The default is ``'DEBUG'``.
        """
        self.logger.setLevel(level)
        for each_handler in self.logger.handlers:
            each_handler.setLevel(level)
        self._level = level

    def _make_child_logger(self, suffix, level):
        """Create a child logger.

        This method uses the ``getChild`` method or copies attributes between the
        ``pymapdl_global`` logger and the new one.
        """
        logger = logging.getLogger(suffix)
        logger.std_out_handler = None
        logger.file_handler = None

        if self.logger.hasHandlers:
            for each_handler in self.logger.handlers:
                new_handler = copy(each_handler)

                if each_handler == self.file_handler:
                    logger.file_handler = new_handler
                elif each_handler == self.std_out_handler:
                    logger.std_out_handler = new_handler

                if level:
                    # The logger handlers are copied and changed the loglevel is
                    # the specified log level is lower than the one of the
                    # global.
                    if each_handler.level > string_to_loglevel[level.upper()]:
                        new_handler.setLevel(level)

                logger.addHandler(new_handler)

        if level:
            if isinstance(level, str):
                level = string_to_loglevel[level.upper()]
            logger.setLevel(level)

        else:
            logger.setLevel(self.logger.level)

        logger.propagate = True
        return logger

    def add_child_logger(self, suffix, level=None):
        """Add a child logger to the main logger.

        This child logger is more general than an instance logger, which is designed to
        track the state of MAPDL instances.

        If the logging level is in the arguments, a new logger with a reference
        to the ``_global`` logger handlers is created instead of a child logger.

        Parameters
        ----------
        suffix : str
            Name of the logger.
        level : str, optional
            Level of logging. The default is ``None``.

        Returns
        -------
        logging.logger
            Logger class.
        """
        name = self.logger.name + "." + suffix
        self._instances[name] = self._make_child_logger(self, name, level)
        return self._instances[name]

    def _add_mapdl_instance_logger(self, name, mapdl_instance, level):
        if isinstance(name, str):
            instance_logger = PymapdlCustomAdapter(self._make_child_logger(name, level), mapdl_instance)
        elif not name:  # pragma: no cover
            instance_logger = PymapdlCustomAdapter(self._make_child_logger("NO_NAMED_YET", level), mapdl_instance)
        else:
            raise ValueError("You can only input 'str' classes to this method.")

        return instance_logger

    def add_instance_logger(self, name, mapdl_instance, level=None):
        """Create a logger for an MAPDL instance.

        The MAPDL instance logger is a logger with an adapter that adds
        contextual information, such as the MAPDL instance name. This logger is
        returned, and you can use it to log events as a normal logger. It is also
        stored in the ``_instances`` field.

        Parameters
        ----------
        name : str
            Name for the new logger.
        mapdl_instance : ansys.mapdl.core.mapdl._MapdlCore
            MAPDL instance object. This should contain the ``name`` attribute.

        Returns
        -------
        ansys.mapdl.core.logging.PymapdlCustomAdapter
            Logger adapter customized to add MAPDL information to the
            logs.  You can use this class to log events in the same
            way you use the ``logger`` class.

        Raises
        ------
        Exception
            You can only input strings as ``name`` to this method.
        """
        count_ = 0
        new_name = name
        while new_name in logging.root.manager.__dict__.keys():
            count_ += 1
            new_name = name + "_" + str(count_)

        self._instances[new_name] = self._add_mapdl_instance_logger(new_name, mapdl_instance, level)
        return self._instances[new_name]

    def __getitem__(self, key):
        if key in self._instances.keys():
            return self._instances[key]
        else:
            raise KeyError(f"There is no instances with name {key}")

    def add_handling_uncaught_expections(self, logger):
        """Redirect the output of an exception to the logger."""

        def handle_exception(exc_type, exc_value, exc_traceback):
            if issubclass(exc_type, KeyboardInterrupt):
                sys.__excepthook__(exc_type, exc_value, exc_traceback)
                return
            logger.critical("Uncaught exception", exc_info=(exc_type, exc_value, exc_traceback))

        sys.excepthook = handle_exception


def addfile_handler(logger, filename=FILE_NAME, level=LOG_LEVEL, write_headers=False):
    """Add a file handler to the input.

    Parameters
    ----------
    logger : logging.Logger or logging.Logger
        Logger to add the file handler to.
    filename : str, optional
        Name of the output file. The default is ``FILE_NAME``.
    level : str, optional
        Level of logging. The default is ``LOG_LEVEL``.
    write_headers : bool, optional
        Whether to write the headers to the file. The default is ``False``.

    Returns
    -------
    logger
        Logger or Logger object.
    """

    file_handler = logging.FileHandler(filename)
    file_handler.setLevel(level)
    file_handler.setFormatter(logging.Formatter(FILE_MSG_FORMAT))

    if isinstance(logger, Logger):
        logger.file_handler = file_handler
        logger.logger.addHandler(file_handler)

    elif isinstance(logger, logging.Logger):
        logger.file_handler = file_handler
        logger.addHandler(file_handler)

    if write_headers:
        file_handler.stream.write(NEW_SESSION_HEADER)
        file_handler.stream.write(DEFAULT_FILE_HEADER)

    return logger


def add_stdout_handler(logger, level=LOG_LEVEL, write_headers=False):
    """Add a file handler to the logger.

    Parameters
    ----------
    logger : logging.Logger or logging.Logger
        Logger to add the file handler to.
    level : str, optional
        Level of logging. The default is ``LOG_LEVEL``, in which
        case ``""DEBUG" is used``.
    write_headers : bool, optional
        Whether to write the headers to the file. The default is ``False``.

    Returns
    -------
    logger
        Logger or Logger object.
    """

    std_out_handler = logging.StreamHandler()
    std_out_handler.setLevel(level)
    std_out_handler.setFormatter(PymapdlFormatter(STDOUT_MSG_FORMAT))

    if isinstance(logger, Logger):
        logger.std_out_handler = std_out_handler
        logger.logger.addHandler(std_out_handler)

    elif isinstance(logger, logging.Logger):
        logger.addHandler(std_out_handler)

    if write_headers:
        std_out_handler.stream.write(DEFAULT_STDOUT_HEADER)

    return logger


LOG = Logger(level=logging.ERROR, to_file=False, to_stdout=True)
LOG.debug("Loaded logging module as LOG")
