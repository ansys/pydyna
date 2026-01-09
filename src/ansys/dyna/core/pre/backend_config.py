# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""
Backend Configuration
=====================

Module for configuring the pre module backend (gRPC vs keywords).

The backend can be selected using the environment variable PYDYNA_PRE_BACKEND:
- "grpc" (default): Use the gRPC server backend
- "keywords": Use the local keywords-based backend

Example usage:
    import os
    os.environ["PYDYNA_PRE_BACKEND"] = "keywords"
    from ansys.dyna.core.pre import launch_dynapre
    solution = launch_dynapre()  # Uses keywords backend
"""

import logging
import os
from enum import Enum
from typing import Optional

logger = logging.getLogger(__name__)

# Environment variable name for backend selection
BACKEND_ENV_VAR = "PYDYNA_PRE_BACKEND"


class BackendType(Enum):
    """Enumeration of available backend types."""

    GRPC = "grpc"
    KEYWORDS = "keywords"


def get_backend_type() -> BackendType:
    """Get the configured backend type from environment variable.

    Returns
    -------
    BackendType
        The configured backend type. Defaults to GRPC if not set.
    """
    backend_str = os.environ.get(BACKEND_ENV_VAR, "grpc").lower()

    if backend_str == "keywords":
        logger.info("Using keywords backend for pre module")
        return BackendType.KEYWORDS
    elif backend_str == "grpc":
        logger.debug("Using gRPC backend for pre module")
        return BackendType.GRPC
    else:
        logger.warning(
            f"Unknown backend type '{backend_str}' specified in {BACKEND_ENV_VAR}. "
            f"Falling back to gRPC backend. Valid options: 'grpc', 'keywords'"
        )
        return BackendType.GRPC


def is_keywords_backend() -> bool:
    """Check if the keywords backend is currently configured.

    Returns
    -------
    bool
        True if keywords backend is configured, False otherwise.
    """
    return get_backend_type() == BackendType.KEYWORDS


def is_grpc_backend() -> bool:
    """Check if the gRPC backend is currently configured.

    Returns
    -------
    bool
        True if gRPC backend is configured, False otherwise.
    """
    return get_backend_type() == BackendType.GRPC


def set_backend(backend: BackendType) -> None:
    """Set the backend type programmatically.

    Parameters
    ----------
    backend : BackendType
        The backend type to use.
    """
    os.environ[BACKEND_ENV_VAR] = backend.value
    logger.info(f"Backend set to: {backend.value}")


# Global state for keywords backend
_keywords_backend_instance: Optional["KeywordsBackend"] = None


def get_keywords_backend() -> "KeywordsBackend":
    """Get or create the global keywords backend instance.

    Returns
    -------
    KeywordsBackend
        The keywords backend instance.
    """
    global _keywords_backend_instance
    if _keywords_backend_instance is None:
        from ansys.dyna.core.pre.keywords_backend import KeywordsBackend

        _keywords_backend_instance = KeywordsBackend()
    return _keywords_backend_instance


def reset_keywords_backend() -> None:
    """Reset the global keywords backend instance.

    This is useful for testing or when starting a new model.
    """
    global _keywords_backend_instance
    _keywords_backend_instance = None
    logger.debug("Keywords backend instance reset")
