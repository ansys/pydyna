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

"""Validation framework for PyDYNA keywords and decks."""

from abc import ABC, abstractmethod
import collections
from enum import Enum
import logging
from typing import TYPE_CHECKING, Callable, List

from ansys.dyna.core.utils.errors import (
    DuplicateIDError,
    DuplicateKeywordError,
    RequiredFieldError,
    ValidationError,
)

if TYPE_CHECKING:
    from ansys.dyna.core.lib.deck import Deck

logger = logging.getLogger(__name__)


class ValidationSeverity(Enum):
    """Severity levels for validation rules."""

    ERROR = "error"
    WARNING = "warning"
    INFO = "info"


class ValidationResult:
    """Result of a validation operation."""

    def __init__(self):
        """Initialize an empty validation result."""
        self.errors: List[ValidationError] = []
        self.warnings: List[ValidationError] = []
        self.info: List[ValidationError] = []

    def add_error(self, error: ValidationError) -> None:
        """Add an error to the result.

        Parameters
        ----------
        error : ValidationError
            The validation error to add.
        """
        if error.severity == ValidationSeverity.ERROR.value:
            self.errors.append(error)
        elif error.severity == ValidationSeverity.WARNING.value:
            self.warnings.append(error)
        else:
            self.info.append(error)

    def has_errors(self) -> bool:
        """Check if there are any errors.

        Returns
        -------
        bool
            True if there are errors, False otherwise.
        """
        return len(self.errors) > 0

    def has_warnings(self) -> bool:
        """Check if there are any warnings.

        Returns
        -------
        bool
            True if there are warnings, False otherwise.
        """
        return len(self.warnings) > 0

    @property
    def is_valid(self) -> bool:
        """Check if validation passed (no errors).

        Returns
        -------
        bool
            True if no errors, False otherwise.
        """
        return not self.has_errors()

    def get_summary(self) -> str:
        """Get a summary of validation results.

        Returns
        -------
        str
            Summary string with counts of errors, warnings, and info messages.
        """
        parts = []
        if self.errors:
            parts.append(f"{len(self.errors)} error(s)")
        if self.warnings:
            parts.append(f"{len(self.warnings)} warning(s)")
        if self.info:
            parts.append(f"{len(self.info)} info message(s)")
        return ", ".join(parts) if parts else "All validations passed"

    def raise_if_errors(self) -> None:
        """Raise an exception if there are any errors.

        Raises
        ------
        ValidationError
            If there are any errors in the result.
        """
        if self.has_errors():
            msg_parts = [f"Validation failed with {len(self.errors)} error(s):"]
            for error in self.errors[:10]:  # Limit to first 10 errors
                msg_parts.append(f"  - {str(error)}")
            if len(self.errors) > 10:
                msg_parts.append(f"  ... and {len(self.errors) - 10} more")
            raise ValidationError("\n".join(msg_parts))


class Validator(ABC):
    """Base class for all validators."""

    def __init__(self, severity: ValidationSeverity = ValidationSeverity.ERROR):
        """Initialize the validator.

        Parameters
        ----------
        severity : ValidationSeverity
            Severity level for violations found by this validator.
        """
        self.severity = severity

    @abstractmethod
    def validate(self, deck: "Deck", result: ValidationResult) -> None:
        """Validate the deck.

        Parameters
        ----------
        deck : Deck
            The deck to validate.
        result : ValidationResult
            Result object to accumulate validation errors.
        """
        pass

    @abstractmethod
    def get_name(self) -> str:
        """Get the name of this validator.

        Returns
        -------
        str
            Validator name for logging and registration.
        """
        pass


class RequiredFieldValidator(Validator):
    """Validator that checks for required fields in keywords."""

    def __init__(self, keyword_pattern: str, field_name: str, severity: ValidationSeverity = ValidationSeverity.ERROR):
        """Initialize the required field validator.

        Parameters
        ----------
        keyword_pattern : str
            Pattern to match keyword types (e.g., "DEFINE_CURVE" matches all DEFINE_CURVE_* keywords).
        field_name : str
            Name of the required field.
        severity : ValidationSeverity
            Severity level for violations.
        """
        super().__init__(severity)
        self.keyword_pattern = keyword_pattern
        self.field_name = field_name

    def get_name(self) -> str:
        """Get the name of this validator."""
        return f"RequiredField[{self.keyword_pattern}.{self.field_name}]"

    def validate(self, deck: "Deck", result: ValidationResult) -> None:
        """Validate that required field exists and is not None.

        Parameters
        ----------
        deck : Deck
            The deck to validate.
        result : ValidationResult
            Result object to accumulate validation errors.
        """
        logger.debug(f"Running {self.get_name()} validator")
        checked_count = 0

        for kwd in deck._keywords:
            if isinstance(kwd, str):
                continue  # Skip string keywords
            # Match by keyword property or class name (remove underscores for comparison)
            matches = False
            if hasattr(kwd, "keyword"):
                # For "DEFINE_CURVE", match if keyword+subkeyword contains the pattern
                full_keyword = f"{kwd.keyword}_{kwd.subkeyword}" if hasattr(kwd, "subkeyword") else kwd.keyword
                matches = (
                    self.keyword_pattern == full_keyword
                    or self.keyword_pattern == kwd.keyword
                    or self.keyword_pattern in full_keyword
                )
            if not matches:
                # Fallback to class name matching (remove underscores)
                kwd_type_normalized = type(kwd).__name__.replace("_", "")
                pattern_normalized = self.keyword_pattern.replace("_", "")
                matches = pattern_normalized.upper() in kwd_type_normalized.upper()

            if matches:
                checked_count += 1
                if hasattr(kwd, self.field_name):
                    value = getattr(kwd, self.field_name)
                    if value is None:
                        error = RequiredFieldError(kwd, self.field_name)
                        error.severity = self.severity.value
                        result.add_error(error)  # add_error auto-sorts by severity
                        logger.warning(f"Required field validation failed: {error}")

        logger.debug(f"{self.get_name()} checked {checked_count} keywords")


class UniqueIDValidator(Validator):
    """Validator that checks for unique ID fields across keyword types."""

    def __init__(self, keyword_type: str, field_name: str, severity: ValidationSeverity = ValidationSeverity.ERROR):
        """Initialize the unique ID validator.

        Parameters
        ----------
        keyword_type : str
            Exact keyword type name (e.g., "SECTION").
        field_name : str
            Name of the ID field that must be unique.
        severity : ValidationSeverity
            Severity level for violations.
        """
        super().__init__(severity)
        self.keyword_type = keyword_type
        self.field_name = field_name

    def get_name(self) -> str:
        """Get the name of this validator."""
        return f"UniqueID[{self.keyword_type}.{self.field_name}]"

    def validate(self, deck: "Deck", result: ValidationResult) -> None:
        """Validate that ID field is unique across keywords of the specified type.

        Parameters
        ----------
        deck : Deck
            The deck to validate.
        result : ValidationResult
            Result object to accumulate validation errors.
        """
        logger.debug(f"Running {self.get_name()} validator")
        ids = []
        keywords = deck[self.keyword_type]  # Returns KeywordCollection

        for kwd in keywords:
            if not hasattr(kwd, self.field_name):
                logger.warning(f"Keyword type {self.keyword_type} does not have field {self.field_name}")
                continue
            id_value = getattr(kwd, self.field_name)
            if id_value is not None:  # Only check non-None values
                ids.append(id_value)

        duplicates = [id_val for id_val, count in collections.Counter(ids).items() if count > 1]
        if duplicates:
            error = DuplicateIDError(self.keyword_type, self.field_name, duplicates)
            error.severity = self.severity.value
            result.add_error(error)  # add_error auto-sorts by severity
            logger.warning(f"Unique ID validation failed: {error}")

        logger.debug(f"{self.get_name()} checked {len(keywords)} keywords, found {len(duplicates)} duplicates")


class KeywordValidator(Validator):
    """Validator that calls _is_valid() on all keywords."""

    def __init__(self, severity: ValidationSeverity = ValidationSeverity.ERROR):
        """Initialize the keyword validator.

        Parameters
        ----------
        severity : ValidationSeverity
            Severity level for violations.
        """
        super().__init__(severity)

    def get_name(self) -> str:
        """Get the name of this validator."""
        return "KeywordValid"

    def validate(self, deck: "Deck", result: ValidationResult) -> None:
        """Validate all keywords using their _is_valid() method.

        Parameters
        ----------
        deck : Deck
            The deck to validate.
        result : ValidationResult
            Result object to accumulate validation errors.
        """
        logger.debug(f"Running {self.get_name()} validator")
        checked_count = 0

        for kwd in deck._keywords:
            if isinstance(kwd, str):
                continue  # Skip string keywords
            checked_count += 1
            is_valid, msg = kwd._is_valid()
            if not is_valid:
                error = ValidationError(f"{kwd} is not valid due to {msg}")
                error.severity = self.severity.value
                result.add_error(error)  # add_error auto-sorts by severity
                logger.warning(f"Keyword validation failed: {error}")

        logger.debug(f"{self.get_name()} checked {checked_count} keywords")


# Keywords that should appear at most once in a deck.
# This list covers common CONTROL keywords and other singleton keywords.
# Users can write custom validators if additional keywords need uniqueness checks.
GLOBALLY_UNIQUE_KEYWORDS = frozenset(
    [
        ("CONTROL", "ACCURACY"),
        ("CONTROL", "BULK_VISCOSITY"),
        ("CONTROL", "CONTACT"),
        ("CONTROL", "CPU"),
        ("CONTROL", "DYNAMIC_RELAXATION"),
        ("CONTROL", "ENERGY"),
        ("CONTROL", "HOURGLASS"),
        ("CONTROL", "IMPLICIT_AUTO"),
        ("CONTROL", "IMPLICIT_DYNAMICS"),
        ("CONTROL", "IMPLICIT_GENERAL"),
        ("CONTROL", "IMPLICIT_SOLUTION"),
        ("CONTROL", "IMPLICIT_SOLVER"),
        ("CONTROL", "OUTPUT"),
        ("CONTROL", "PARALLEL"),
        ("CONTROL", "SHELL"),
        ("CONTROL", "SOLID"),
        ("CONTROL", "SOLUTION"),
        ("CONTROL", "TERMINATION"),
        ("CONTROL", "THERMAL_SOLVER"),
        ("CONTROL", "THERMAL_TIMESTEP"),
        ("CONTROL", "TIMESTEP"),
    ]
)


class GloballyUniqueKeywordValidator(Validator):
    """Validator that checks for keywords that should appear at most once in a deck."""

    def __init__(self, severity: ValidationSeverity = ValidationSeverity.ERROR):
        """Initialize the globally unique keyword validator.

        Parameters
        ----------
        severity : ValidationSeverity
            Severity level for violations.
        """
        super().__init__(severity)

    def get_name(self) -> str:
        """Get the name of this validator."""
        return "GloballyUniqueKeyword"

    def validate(self, deck: "Deck", result: ValidationResult) -> None:
        """Validate that globally unique keywords appear at most once.

        Parameters
        ----------
        deck : Deck
            The deck to validate.
        result : ValidationResult
            Result object to accumulate validation errors.
        """
        logger.debug(f"Running {self.get_name()} validator")
        keyword_counts: dict = {}

        for kwd in deck._keywords:
            if isinstance(kwd, str):
                continue
            key = (getattr(kwd, "keyword", None), getattr(kwd, "subkeyword", None))
            if key in GLOBALLY_UNIQUE_KEYWORDS:
                keyword_counts[key] = keyword_counts.get(key, 0) + 1

        duplicates_found = 0
        for (keyword_type, subkeyword), count in keyword_counts.items():
            if count > 1:
                duplicates_found += 1
                error = DuplicateKeywordError(keyword_type, subkeyword, count)
                error.severity = self.severity.value
                result.add_error(error)
                logger.warning(f"Globally unique keyword validation failed: {error}")

        logger.debug(
            f"{self.get_name()} checked {len(keyword_counts)} unique keyword types, found {duplicates_found} duplicates"
        )


class CustomValidator(Validator):
    """Validator that wraps a custom validation function."""

    def __init__(
        self,
        name: str,
        func: Callable[["Deck", ValidationResult], None],
        severity: ValidationSeverity = ValidationSeverity.ERROR,
    ):
        """Initialize a custom validator.

        Parameters
        ----------
        name : str
            Name of the validator.
        func : Callable
            Validation function that takes (deck, result) and adds errors to result.
        severity : ValidationSeverity
            Default severity level for violations.
        """
        super().__init__(severity)
        self._name = name
        self.func = func

    def get_name(self) -> str:
        """Get the name of this validator."""
        return self._name

    def validate(self, deck: "Deck", result: ValidationResult) -> None:
        """Run the custom validation function.

        Parameters
        ----------
        deck : Deck
            The deck to validate.
        result : ValidationResult
            Result object to accumulate validation errors.
        """
        logger.debug(f"Running custom validator: {self._name}")
        self.func(deck, result)


class ValidatorRegistry:
    """Registry for managing validators."""

    def __init__(self):
        """Initialize the validator registry."""
        self._validators: List[Validator] = []
        self._default_validators_registered = False

    def register(self, validator: Validator) -> None:
        """Register a validator.

        Parameters
        ----------
        validator : Validator
            The validator to register.
        """
        logger.debug(f"Registering validator: {validator.get_name()}")
        self._validators.append(validator)

    def register_custom(
        self,
        name: str,
        func: Callable[["Deck", ValidationResult], None],
        severity: ValidationSeverity = ValidationSeverity.ERROR,
    ) -> None:
        """Register a custom validation function.

        Parameters
        ----------
        name : str
            Name of the validator.
        func : Callable
            Validation function that takes (deck, result) and adds errors to result.
        severity : ValidationSeverity
            Default severity level for violations.
        """
        validator = CustomValidator(name, func, severity)
        self.register(validator)

    def unregister(self, validator_name: str) -> bool:
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
        original_length = len(self._validators)
        self._validators = [v for v in self._validators if v.get_name() != validator_name]
        removed = len(self._validators) < original_length
        if removed:
            logger.debug(f"Unregistered validator: {validator_name}")
        return removed

    def clear(self) -> None:
        """Remove all validators from the registry."""
        logger.debug(f"Clearing {len(self._validators)} validators")
        self._validators.clear()
        self._default_validators_registered = False

    def get_all(self) -> List[Validator]:
        """Get all registered validators.

        Returns
        -------
        List[Validator]
            List of all registered validators.
        """
        return self._validators.copy()

    def validate(self, deck: "Deck") -> ValidationResult:
        """Run all registered validators on a deck.

        Parameters
        ----------
        deck : Deck
            The deck to validate.

        Returns
        -------
        ValidationResult
            Result containing all errors, warnings, and info messages.
        """
        result = ValidationResult()
        logger.info(f"Running {len(self._validators)} validators on deck")

        for validator in self._validators:
            try:
                validator.validate(deck, result)
            except Exception as e:
                logger.error(f"Validator {validator.get_name()} raised exception: {e}", exc_info=True)
                error = ValidationError(
                    f"Validator '{validator.get_name()}' failed with exception: {e}",
                    severity=ValidationSeverity.ERROR.value,
                )
                result.add_error(error)

        logger.info(f"Validation complete: {result.get_summary()}")
        return result

    def register_default_validators(self) -> None:
        """Register the default set of validators for LS-DYNA decks."""
        if self._default_validators_registered:
            logger.debug("Default validators already registered, skipping")
            return

        logger.info("Registering default validators")

        # Keyword _is_valid() validator (legacy check)
        self.register(KeywordValidator(ValidationSeverity.ERROR))

        # Required field validators for DEFINE_CURVE_* keywords
        self.register(RequiredFieldValidator("DEFINE_CURVE", "lcid", ValidationSeverity.ERROR))

        # Required field validators for SECTION_* keywords
        self.register(RequiredFieldValidator("SECTION", "secid", ValidationSeverity.ERROR))

        # Unique ID validators
        self.register(UniqueIDValidator("SECTION", "secid", ValidationSeverity.ERROR))

        # Globally unique keyword validator (e.g., CONTROL_TIMESTEP should appear at most once)
        self.register(GloballyUniqueKeywordValidator(ValidationSeverity.ERROR))

        # Note: We don't enforce unique lcid for DEFINE_CURVE because multiple curves
        # can legally share the same ID in some LS-DYNA workflows

        self._default_validators_registered = True
        logger.info(f"Registered {len(self._validators)} default validators")
