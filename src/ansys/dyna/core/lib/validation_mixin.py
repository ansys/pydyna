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

"""Mixin providing validation functionality for Deck."""

from typing import Callable, List

from ansys.dyna.core.lib.validators import ValidationResult, Validator, ValidatorRegistry


class ValidationMixin:
    """Mixin that provides validation capabilities for Deck.

    This mixin adds methods for validating keywords in a deck using a registry
    of validators. Default validators check for required fields and unique IDs.

    Attributes expected from the host class:
        _validator_registry: ValidatorRegistry instance
    """

    # Type hint for the registry - will be initialized in __init__ of host class
    _validator_registry: ValidatorRegistry

    def _init_validation(self) -> None:
        """Initialize the validator registry. Call this from host class __init__."""
        self._validator_registry = ValidatorRegistry()

    def validate(self) -> ValidationResult:
        """Validate the collection of keywords.

        Returns
        -------
        ValidationResult
            Result object containing errors, warnings, and info messages.

        Examples
        --------
        >>> result = deck.validate()
        >>> if result.has_errors():
        ...     print(result.get_summary())
        ...     for error in result.errors:
        ...         print(f"  - {error}")
        """
        # Ensure default validators are registered
        if len(self._validator_registry.get_all()) == 0:
            self._validator_registry.register_default_validators()
        return self._validator_registry.validate(self)

    def register_validator(self, validator: Validator) -> None:
        """Register a custom validator.

        Parameters
        ----------
        validator : Validator
            The validator to register.

        Examples
        --------
        >>> from ansys.dyna.core.lib.validators import RequiredFieldValidator, ValidationSeverity
        >>> validator = RequiredFieldValidator("MAT", "mid", ValidationSeverity.ERROR)
        >>> deck.register_validator(validator)
        """
        self._validator_registry.register(validator)

    def register_custom_validator(self, name: str, func: Callable[["ValidationMixin", ValidationResult], None]) -> None:
        """Register a custom validation function.

        Parameters
        ----------
        name : str
            Name of the validator.
        func : Callable
            Validation function that takes (deck, result) and adds errors to result.

        Examples
        --------
        >>> def check_part_ids(deck, result):
        ...     from ansys.dyna.core.utils.errors import ValidationError
        ...     # Custom validation logic
        ...     if some_condition:
        ...         error = ValidationError("Custom error message")
        ...         result.add_error(error)
        >>> deck.register_custom_validator("part_id_check", check_part_ids)
        """
        self._validator_registry.register_custom(name, func)

    def unregister_validator(self, validator_name: str) -> bool:
        """Unregister a validator by name.

        Parameters
        ----------
        validator_name : str
            Name of the validator to remove.

        Returns
        -------
        bool
            True if validator was found and removed, False otherwise.
        """
        return self._validator_registry.unregister(validator_name)

    def clear_validators(self) -> None:
        """Remove all validators from the registry."""
        self._validator_registry.clear()

    def get_validators(self) -> List[Validator]:
        """Get all registered validators.

        Returns
        -------
        List[Validator]
            List of all registered validators.
        """
        return self._validator_registry.get_all()
