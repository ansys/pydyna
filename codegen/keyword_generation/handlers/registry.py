# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
"""
Handler Registry: Manages registration and execution of keyword data handlers.

This module provides a centralized registry for all keyword transformation handlers,
ensuring they are executed in the correct order and with proper lifecycle management.

IMPORTANT: Handler execution order is critical. The registry maintains an explicit
ordering (defined in create_default_registry) that ensures handlers run in the
correct sequence. For example, card-set must run before conditional-card to allow
conditional-card to modify cards via shared references.

Labels are stored as object references (not indices), so handlers can freely
reorder, insert, or remove cards without invalidating previously registered labels.

See agents/codegen.md for detailed documentation on handler ordering and semantics.
"""

import collections
import logging
import typing
from typing import Dict, Optional

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
from keyword_generation.handlers.handler_base import (
    HandlerMetadata,
    KeywordHandler,
    get_all_handler_metadata,
    validate_handler_settings,
)

logger = logging.getLogger(__name__)


class HandlerRegistry:
    """
    Registry for keyword transformation handlers.

    Manages the registration, ordering, and execution of handlers that transform
    keyword data during code generation. Handlers are executed in explicit
    registration order as defined in create_default_registry().
    """

    def __init__(self):
        """Initialize the handler registry."""
        self._handlers: collections.OrderedDict[str, KeywordHandler] = collections.OrderedDict()
        self._metadata: Dict[str, HandlerMetadata] = {}

    def register(self, name: str, handler: KeywordHandler, metadata: typing.Optional[HandlerMetadata] = None) -> None:
        """
        Register a handler with a given name.

        Args:
            name: The configuration key name for this handler (e.g., "reorder-card")
            handler: The handler instance to register
            metadata: Optional handler metadata (if not provided, retrieved from decorator)
        """
        self._handlers[name] = handler
        if metadata:
            self._metadata[name] = metadata
        elif hasattr(handler.__class__, "_handler_metadata"):
            self._metadata[name] = handler.__class__._handler_metadata  # type: ignore[attr-defined]
        logger.debug(f"Registered handler '{name}': {handler.__class__.__name__}")

    def apply_all(
        self,
        kwd_data: KeywordData,
        settings: typing.Dict[str, typing.Any],
        validate: bool = True,
        initial_labels: Optional[Dict[str, int]] = None,
    ) -> None:
        """
        Apply all registered handlers to keyword data in registration order.

        Handlers are executed in explicit registration order (defined in create_default_registry).
        Only handlers with corresponding settings in the configuration are executed.

        The LabelRegistry is initialized before handlers run, mapping label names to
        card objects. Since labels reference objects (not indices), the registry
        remains valid even after handlers reorder or insert cards.

        Args:
            kwd_data: The keyword data structure to transform
            settings: Configuration settings containing handler-specific options
            validate: If True, validate settings against handler schemas before execution
            initial_labels: Optional dict mapping label names to card indices from manifest
        """
        # Determine which handlers need to run based on settings
        handlers_to_run = set()
        for handler_name in self._handlers.keys():
            if settings.get(handler_name) is not None:
                handlers_to_run.add(handler_name)

        if not handlers_to_run:
            logger.debug("No handlers to run (no settings provided)")
            return

        logger.debug(f"Handlers to run based on settings: {handlers_to_run}")

        # Validate settings if requested
        if validate:
            for handler_name in handlers_to_run:
                try:
                    validate_handler_settings(handler_name, settings[handler_name])
                except ValueError as e:
                    logger.error(f"Validation failed for handler '{handler_name}': {e}")
                    # Continue with other handlers even if one fails validation
                    # This preserves backward compatibility
                except ImportError:
                    logger.debug("Validation skipped (jsonschema not available)")
                    break  # Skip validation for remaining handlers

        # Use registration order (which preserves the original explicit ordering)
        # instead of topological sort for now
        sorted_names = [name for name in self._handlers.keys() if name in handlers_to_run]

        # Initialize label registry before running any handlers.
        # Since labels reference card objects (not indices), the registry remains valid
        # even after handlers reorder, insert, or remove cards.
        keyword_name = f"{kwd_data.keyword}.{kwd_data.subkeyword}"
        labels = LabelRegistry.from_cards(kwd_data.cards, keyword=keyword_name, initial_labels=initial_labels)
        kwd_data.label_registry = labels
        logger.debug(f"Initialized LabelRegistry for {keyword_name} with {len(labels.get_all_labels())} labels")

        # Execute handlers in sorted order
        for handler_name in sorted_names:
            handler = self._handlers[handler_name]
            handler_settings = settings[handler_name]
            logger.debug(f"Applying handler '{handler_name}'")

            # Run the handler
            handler.handle(kwd_data, handler_settings)

    def post_process_all(self, kwd_data: KeywordData) -> None:
        """
        Run post-processing for all registered handlers.

        Post-processing is an optional finalization phase that runs after all handlers
        have completed their main processing. Handlers only need to override post_process
        if they require operations that depend on the combined effects of all handlers.

        Currently used by:
        - shared-field: Processes deferred negative-index shared fields after options exist
        - rename-property: Detects property name collisions after all renames complete

        Runs for all registered handlers (not just those with settings) in registration
        order. Handlers with no post-processing needs use the default no-op implementation
        from the base class.

        Args:
            kwd_data: Keyword data structure (contains label_registry if initialized)
        """
        logger.debug(f"Running post-processing for {len(self._handlers)} handlers")
        for handler_name, handler in self._handlers.items():
            if isinstance(handler, KeywordHandler):
                logger.debug(f"Post-processing handler '{handler_name}'")
                handler.post_process(kwd_data)

    def get_handler_names(self) -> typing.List[str]:
        """
        Get list of all registered handler names.

        Returns
        -------
            List of handler configuration key names in registration order
        """
        return list(self._handlers.keys())


def discover_handlers() -> Dict[str, HandlerMetadata]:
    """
    Automatically discover all handlers with @handler decorator.

    Scans the handlers package for modules, imports them, and collects
    metadata from handlers decorated with @handler.

    Returns
    -------
        Dictionary mapping handler names to their metadata
    """
    import importlib
    import pkgutil

    import keyword_generation.handlers as handlers_package

    logger.debug("Discovering handlers in handlers package")

    # Get the path to the handlers package
    package_path = handlers_package.__path__

    # Iterate through all modules in the handlers package
    for _, module_name, _ in pkgutil.iter_modules(package_path):
        if module_name in ("handler_base", "registry", "__pycache__"):
            continue

        try:
            # Import the module to trigger decorator execution
            full_module_name = f"keyword_generation.handlers.{module_name}"
            logger.debug(f"Importing handler module: {full_module_name}")
            importlib.import_module(full_module_name)
        except Exception as e:
            logger.warning(f"Failed to import handler module {module_name}: {e}")

    # Get all discovered handler metadata from the global registry
    all_metadata = get_all_handler_metadata()
    logger.debug(f"Discovered {len(all_metadata)} handlers via auto-discovery")

    return all_metadata


def create_default_registry() -> HandlerRegistry:
    """
    Create and populate the default handler registry.

    Handlers are auto-discovered by scanning the handlers package for
    classes decorated with @handler.

    Returns
    -------
        Configured HandlerRegistry with all standard handlers
    """
    logger.debug("Creating default handler registry")
    registry = HandlerRegistry()

    # Auto-discover handlers using decorator metadata
    handler_metadata = discover_handlers()

    # Register handlers in explicit order
    # This order is critical - some handlers depend on others having run first
    handler_order = [
        "reorder-card",
        "skip-card",  # Must run before insert-card so refs resolve to original cards
        "insert-card",  # Moved before table-card so inserted cards can be referenced
        "table-card",
        "override-field",
        "replace-card",
        "series-card",
        "add-option",
        "card-set",
        "conditional-card",
        "cascading-card",  # Must run after conditional-card to not conflict with func
        "rename-property",
        "table-card-group",
        "external-card-implementation",
        "shared-field",
    ]

    for name in handler_order:
        if name in handler_metadata:
            metadata = handler_metadata[name]
            try:
                handler_instance = metadata.handler_class()
                registry.register(name, handler_instance, metadata)
                logger.debug(f"Auto-registered handler: {name}")
            except Exception as e:
                logger.error(f"Failed to instantiate handler {name}: {e}", exc_info=True)

    # Register any handlers not in the explicit order (for extensibility)
    for name, metadata in handler_metadata.items():
        if name not in handler_order:
            try:
                handler_instance = metadata.handler_class()
                registry.register(name, handler_instance, metadata)
                logger.warning(f"Auto-registered handler '{name}' not in explicit order - will run last")
            except Exception as e:
                logger.error(f"Failed to instantiate handler {name}: {e}", exc_info=True)

    logger.debug(f"Auto-registered {len(registry.get_handler_names())} handlers")
    return registry
