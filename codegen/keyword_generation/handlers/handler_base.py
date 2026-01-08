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
Handler base classes and metadata system for keyword code generation.

This module provides the abstract base class for all handlers and the decorator-based
metadata system for handler registration and documentation.
"""

import abc
from dataclasses import dataclass
import logging
from typing import Any, Dict, List, Optional, Type

from keyword_generation.data_model.keyword_data import KeywordData

logger = logging.getLogger(__name__)


@dataclass
class HandlerMetadata:
    """
    Metadata for a keyword handler.

    Attributes:
        name: Handler name (used as key in manifest.json configuration)
        handler_class: The handler class itself
        phase: Execution phase ('handle' or 'post_process')
        description: Human-readable description of handler's purpose
        input_schema: JSON schema describing expected settings structure (optional)
        output_description: Description of what the handler adds/modifies in kwd_data
    """

    name: str
    handler_class: Type["KeywordHandler"]
    phase: str = "handle"
    description: str = ""
    input_schema: Optional[Dict[str, Any]] = None
    output_description: str = ""


# Global registry of handler metadata
_HANDLER_METADATA: Dict[str, HandlerMetadata] = {}


def handler(
    name: str,
    phase: str = "handle",
    description: str = "",
    input_schema: Optional[Dict[str, Any]] = None,
    output_description: str = "",
):
    """
    Decorator to register a handler class with metadata.

    This decorator captures essential information about each handler, enabling:
    - Automatic handler discovery and registration
    - Runtime validation of configuration settings
    - Comprehensive documentation of handler behavior

    Note: Handler execution order is determined by explicit registration order in
    create_default_registry(), not by declared dependencies. This ensures predictable
    behavior since handlers use reference semantics with shared data structures.

    Args:
        name: Handler name matching the key in manifest.json "generation-options"
        phase: Execution phase - 'handle' (main processing) or 'post_process' (finalization)
        description: Clear explanation of what this handler does
        input_schema: JSON Schema dict describing the expected settings structure
        output_description: Description of what keys/fields the handler adds to kwd_data

    Returns:
        Decorator function that registers the handler class

    Example:
        @handler(
            name="conditional-card",
            description="Adds conditional logic to cards based on field values",
            input_schema={
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "index": {"type": "integer"},
                        "func": {"type": "string"}
                    },
                    "required": ["index", "func"]
                }
            },
            output_description="Adds 'func' property to card dict"
        )
        class ConditionalCardHandler(KeywordHandler):
            ...
    """

    def decorator(cls: Type["KeywordHandler"]) -> Type["KeywordHandler"]:
        """Inner decorator function that stores metadata and returns class unchanged."""
        logger.debug(f"Registering handler '{name}' from class {cls.__name__}")

        metadata = HandlerMetadata(
            name=name,
            handler_class=cls,
            phase=phase,
            description=description,
            input_schema=input_schema,
            output_description=output_description,
        )

        _HANDLER_METADATA[name] = metadata
        # Store metadata on class for easy access
        cls._handler_metadata = metadata  # type: ignore[attr-defined]

        logger.debug(f"Handler '{name}' registered (phase={phase})")

        return cls

    return decorator


def get_handler_metadata(name: str) -> Optional[HandlerMetadata]:
    """
    Retrieve metadata for a registered handler by name.

    Args:
        name: Handler name

    Returns:
        HandlerMetadata if found, None otherwise
    """
    return _HANDLER_METADATA.get(name)


def get_all_handler_metadata() -> Dict[str, HandlerMetadata]:
    """
    Retrieve metadata for all registered handlers.

    Returns:
        Dictionary mapping handler names to their metadata
    """
    return _HANDLER_METADATA.copy()


def validate_handler_settings(handler_name: str, settings: List[Dict[str, Any]]) -> None:
    """
    Validate handler settings against its JSON schema.

    Uses the JSON schema defined in the handler's metadata to validate
    the settings from manifest.json. Provides clear error messages for
    invalid configurations.

    Args:
        handler_name: Name of the handler
        settings: List of setting dictionaries from manifest.json

    Raises:
        ValueError: If settings are invalid or handler not found
        ImportError: If jsonschema package is not available
    """
    try:
        import jsonschema
    except ImportError:
        logger.warning("jsonschema package not installed, skipping validation")
        return

    metadata = get_handler_metadata(handler_name)
    if not metadata:
        raise ValueError(f"Handler '{handler_name}' not found in registry")

    if not metadata.input_schema:
        logger.debug(f"No input schema defined for handler '{handler_name}', skipping validation")
        return

    try:
        jsonschema.validate(instance=settings, schema=metadata.input_schema)
        logger.debug(f"Settings for handler '{handler_name}' validated successfully")
    except jsonschema.ValidationError as e:
        error_msg = f"Invalid settings for handler '{handler_name}': {e.message}"
        logger.error(error_msg, exc_info=True)
        raise ValueError(error_msg) from e
    except jsonschema.SchemaError as e:
        error_msg = f"Invalid schema for handler '{handler_name}': {e.message}"
        logger.error(error_msg, exc_info=True)
        raise ValueError(error_msg) from e


class KeywordHandler(metaclass=abc.ABCMeta):
    """
    Abstract base class for keyword handlers.

    Handlers transform keyword data structures during code generation. Each handler
    receives the keyword data dictionary and configuration settings, modifies the
    data as needed, and returns control to the registry for the next handler.

    Subclasses must implement:
        - handle(): Main transformation logic

    Subclasses may optionally override:
        - post_process(): Finalization logic (runs after all handlers complete)
          Default implementation is a no-op. Only override if you need to perform
          operations that depend on the combined effects of all handlers.

    Subclasses should use the @handler decorator to provide metadata including
    name, dependencies, and documentation.
    """

    @abc.abstractmethod
    def handle(self, kwd_data: KeywordData, settings: List[Dict[str, Any]]) -> None:
        """
        Transform keyword data based on settings.

        This is the main entry point for handler logic. It receives the complete
        keyword data structure and handler-specific settings from manifest.json,
        and modifies kwd_data in place.

        Handlers can access kwd_data.label_registry for position-independent card
        referencing. Note that label_registry may be None for handlers that run
        before reorder-card (when card positions aren't finalized yet).

        Args:
            kwd_data: KeywordData instance containing keyword metadata, cards, fields,
                      and label_registry for card referencing
            settings: List of handler-specific setting dictionaries from manifest.json "generation-options"

        Raises:
            NotImplementedError: Must be implemented by subclass
        """
        raise NotImplementedError

    def post_process(self, kwd_data: KeywordData) -> None:
        """
        Optional finalization logic that runs after all handlers have executed.

        This phase is useful for cleanup, validation, or transformations that
        depend on the combined effects of all handlers. The default implementation
        is a no-op.

        Override this method only if your handler needs to:
        - Process data that depends on other handlers' modifications
        - Perform validation that requires the complete transformed structure
        - Clean up or finalize state after all transformations

        Current handlers using post_process:
        - shared-field: Processes deferred negative-index shared fields after options exist
        - rename-property: Detects property name collisions after all renames complete

        Handlers can access kwd_data.label_registry if needed.

        Args:
            kwd_data: KeywordData instance after all handle() calls
        """
        pass
