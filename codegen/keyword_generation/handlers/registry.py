# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
"""
Handler Registry: Manages registration and execution of keyword data handlers.

This module provides a centralized registry for all keyword transformation handlers,
ensuring they are executed in the correct order (based on dependencies) and with
proper lifecycle management.

IMPORTANT: Handler execution order is critical. The registry maintains a specific
ordering (defined in create_default_registry) that ensures handlers run in the
correct sequence. For example, reorder-card must run before handlers that use
positional indices, and card-set must run before conditional-card to allow
conditional-card to modify cards via shared references.

See agents/codegen.md for detailed documentation on handler ordering and semantics.
"""
import collections
import logging
import typing
from typing import Dict, List, Set

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.handlers.handler_base import (
    HandlerMetadata,
    KeywordHandler,
    get_all_handler_metadata,
    validate_handler_settings,
)

logger = logging.getLogger(__name__)


class CyclicDependencyError(Exception):
    """Raised when handlers have circular dependencies."""

    pass


class HandlerRegistry:
    """
    Registry for keyword transformation handlers.

    Manages the registration, ordering, and execution of handlers that transform
    keyword data during code generation. Handlers are automatically ordered based
    on their declared dependencies using topological sorting.
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

    def _topological_sort(self, handler_names: Set[str]) -> List[str]:
        """
        Sort handlers based on dependencies using topological sort.

        Args:
            handler_names: Set of handler names to sort

        Returns:
            List of handler names in dependency-respecting order

        Raises:
            CyclicDependencyError: If circular dependencies are detected
        """
        logger.debug(f"Performing topological sort on {len(handler_names)} handlers")

        # Build adjacency list and in-degree count
        in_degree: Dict[str, int] = {name: 0 for name in handler_names}
        adjacency: Dict[str, List[str]] = {name: [] for name in handler_names}

        for name in handler_names:
            metadata = self._metadata.get(name)
            if metadata:
                for dep in metadata.dependencies:
                    if dep in handler_names:
                        adjacency[dep].append(name)
                        in_degree[name] += 1
                    elif dep not in self._handlers:
                        # Only warn if the dependency handler doesn't exist at all
                        logger.warning(f"Handler '{name}' depends on '{dep}' which is not registered")
                    # If dep is in self._handlers but not in handler_names, that's fine
                    # (it just means the dependency isn't being run this time)

        # Kahn's algorithm for topological sort
        queue = collections.deque([name for name in handler_names if in_degree[name] == 0])
        result = []

        while queue:
            current = queue.popleft()
            result.append(current)

            for neighbor in adjacency[current]:
                in_degree[neighbor] -= 1
                if in_degree[neighbor] == 0:
                    queue.append(neighbor)

        # Check for cycles
        if len(result) != len(handler_names):
            remaining = handler_names - set(result)
            logger.error(f"Cyclic dependency detected among handlers: {remaining}")
            raise CyclicDependencyError(f"Cyclic dependency detected among handlers: {remaining}")

        logger.debug(f"Topological sort result: {result}")
        return result

    def apply_all(self, kwd_data: KeywordData, settings: typing.Dict[str, typing.Any], validate: bool = True) -> None:
        """
        Apply all registered handlers to keyword data in dependency order.

        Handlers are executed in an order that respects their declared dependencies.
        Only handlers with corresponding settings in the configuration are executed.

        Args:
            kwd_data: The keyword data structure to transform
            settings: Configuration settings containing handler-specific options
            validate: If True, validate settings against handler schemas before execution
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

        # Execute handlers in sorted order
        for handler_name in sorted_names:
            handler = self._handlers[handler_name]
            handler_settings = settings[handler_name]
            logger.debug(f"Applying handler '{handler_name}'")
            handler.handle(kwd_data, handler_settings)

    def post_process_all(self, kwd_data: KeywordData) -> None:
        """
        Run post-processing for all handlers that require it.

        Post-processing runs for all registered handlers (not just those with settings)
        in registration order.

        Args:
            kwd_data: Keyword data structure
        """
        logger.debug(f"Running post-processing for {len(self._handlers)} handlers")
        for handler_name, handler in self._handlers.items():
            if isinstance(handler, KeywordHandler):
                logger.debug(f"Post-processing handler '{handler_name}'")
                handler.post_process(kwd_data)

    def get_handler_names(self) -> typing.List[str]:
        """
        Get list of all registered handler names.

        Returns:
            List of handler configuration key names in registration order
        """
        return list(self._handlers.keys())

    def get_execution_order(self, settings: typing.Dict[str, typing.Any]) -> List[str]:
        """
        Get the execution order for handlers given specific settings.

        This is useful for debugging and understanding handler dependencies.

        Args:
            settings: Configuration settings to determine which handlers run

        Returns:
            List of handler names in execution order
        """
        handlers_to_run = {name for name in self._handlers.keys() if settings.get(name) is not None}
        if not handlers_to_run:
            return []
        try:
            return self._topological_sort(handlers_to_run)
        except CyclicDependencyError:
            return [name for name in self._handlers.keys() if name in handlers_to_run]


def discover_handlers() -> Dict[str, HandlerMetadata]:
    """
    Automatically discover all handlers with @handler decorator.

    Scans the handlers package for modules, imports them, and collects
    metadata from handlers decorated with @handler.

    Returns:
        Dictionary mapping handler names to their metadata
    """
    import importlib
    import pkgutil

    import keyword_generation.handlers as handlers_package

    logger.debug("Discovering handlers in handlers package")
    discovered = {}

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

    Returns:
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
        "table-card",
        "override-field",
        "replace-card",
        "insert-card",
        "series-card",
        "add-option",
        "card-set",
        "conditional-card",
        "rename-property",
        "skip-card",
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
