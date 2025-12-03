# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
"""
Handler Registry: Manages registration and execution of keyword data handlers.

This module provides a centralized registry for all keyword transformation handlers,
ensuring they are executed in the correct order and with proper lifecycle management.
"""
import collections
import typing

from keyword_generation.handlers.handler_base import KeywordHandler


class HandlerRegistry:
    """Registry for keyword transformation handlers.

    Manages the registration, ordering, and execution of handlers that transform
    keyword data during code generation. Handlers are executed in registration order.
    """

    def __init__(self):
        self._handlers = collections.OrderedDict()

    def register(self, name: str, handler: KeywordHandler) -> None:
        """Register a handler with a given name.

        Args:
            name: The configuration key name for this handler (e.g., "reorder-card")
            handler: The handler instance to register
        """
        self._handlers[name] = handler

    def apply_all(self, kwd_data: typing.Dict[str, typing.Any], settings: typing.Dict[str, typing.Any]) -> None:
        """Apply all registered handlers to keyword data.

        Args:
            kwd_data: The keyword data dictionary to transform
            settings: Configuration settings containing handler-specific options
        """
        for handler_name, handler in self._handlers.items():
            handler_settings = settings.get(handler_name)
            if handler_settings is not None:
                handler.handle(kwd_data, handler_settings)

    def post_process_all(self, kwd_data: typing.Dict[str, typing.Any]) -> None:
        """Run post-processing for all handlers that require it.

        Args:
            kwd_data: The keyword data dictionary to post-process
        """
        for handler in self._handlers.values():
            if isinstance(handler, KeywordHandler):
                handler.post_process(kwd_data)

    def get_handler_names(self) -> typing.List[str]:
        """Get list of all registered handler names.

        Returns:
            List of handler configuration key names
        """
        return list(self._handlers.keys())


def create_default_registry() -> HandlerRegistry:
    """Create and populate the default handler registry.

    Handlers are registered in the order they should be applied. Order matters
    for some handlers (e.g., reorder-card should run early).

    Returns:
        Configured HandlerRegistry with all standard handlers
    """
    from keyword_generation.handlers.add_option import AddOptionHandler
    from keyword_generation.handlers.card_set import CardSetHandler
    from keyword_generation.handlers.conditional_card import ConditionalCardHandler
    from keyword_generation.handlers.external_card import ExternalCardHandler
    from keyword_generation.handlers.insert_card import InsertCardHandler
    from keyword_generation.handlers.override_field import OverrideFieldHandler
    from keyword_generation.handlers.override_subkeyword import OverrideSubkeywordHandler
    from keyword_generation.handlers.rename_property import RenamePropertyHandler
    from keyword_generation.handlers.reorder_card import ReorderCardHandler
    from keyword_generation.handlers.replace_card import ReplaceCardHandler
    from keyword_generation.handlers.series_card import SeriesCardHandler
    from keyword_generation.handlers.shared_field import SharedFieldHandler
    from keyword_generation.handlers.skip_card import SkipCardHandler
    from keyword_generation.handlers.table_card import TableCardHandler
    from keyword_generation.handlers.table_card_group import TableCardGroupHandler

    registry = HandlerRegistry()

    # Order matters - reorder-card should run first
    registry.register("reorder-card", ReorderCardHandler())
    registry.register("table-card", TableCardHandler())
    registry.register("override-field", OverrideFieldHandler())
    registry.register("replace-card", ReplaceCardHandler())
    registry.register("insert-card", InsertCardHandler())
    registry.register("series-card", SeriesCardHandler())
    registry.register("add-option", AddOptionHandler())
    registry.register("card-set", CardSetHandler())
    registry.register("conditional-card", ConditionalCardHandler())
    registry.register("rename-property", RenamePropertyHandler())
    registry.register("skip-card", SkipCardHandler())
    registry.register("table-card-group", TableCardGroupHandler())
    registry.register("external-card-implementation", ExternalCardHandler())
    registry.register("shared-field", SharedFieldHandler())
    registry.register("override-subkeyword", OverrideSubkeywordHandler())

    return registry
