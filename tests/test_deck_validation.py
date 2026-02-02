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

"""Tests for deck validation functionality."""

import tempfile
from pathlib import Path

import pytest

from ansys.dyna.core.keywords.keyword_classes.auto import DefineCurve, SectionShell
from ansys.dyna.core.keywords.keyword_classes.auto.control.control_timestep import ControlTimestep
from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.lib.validators import (
    CustomValidator,
    GloballyUniqueKeywordValidator,
    RequiredFieldValidator,
    UniqueIDValidator,
    ValidationResult,
    ValidationSeverity,
    ValidatorRegistry,
)
from ansys.dyna.core.pre.errors import (
    DuplicateIDError,
    DuplicateKeywordError,
    RequiredFieldError,
    ValidationError,
)


class TestValidationResult:
    """Test ValidationResult class."""

    def test_empty_result(self):
        """Test empty validation result."""
        result = ValidationResult()
        assert result.is_valid
        assert not result.has_errors()
        assert not result.has_warnings()
        assert result.get_summary() == "All validations passed"

    def test_add_error(self):
        """Test adding errors to result."""
        result = ValidationResult()
        error = ValidationError("Test error", severity="error")
        result.add_error(error)

        assert not result.is_valid
        assert result.has_errors()
        assert len(result.errors) == 1
        assert result.errors[0] == error

    def test_add_warning(self):
        """Test adding warnings to result."""
        result = ValidationResult()
        warning = ValidationError("Test warning", severity="warning")
        result.add_error(warning)

        assert result.is_valid  # Warnings don't affect validity
        assert result.has_warnings()
        assert len(result.warnings) == 1

    def test_get_summary(self):
        """Test summary generation."""
        result = ValidationResult()
        result.add_error(ValidationError("Error 1", severity="error"))
        result.add_error(ValidationError("Error 2", severity="error"))
        result.add_error(ValidationError("Warning 1", severity="warning"))

        summary = result.get_summary()
        assert "2 error(s)" in summary
        assert "1 warning(s)" in summary

    def test_raise_if_errors(self):
        """Test raising exception on errors."""
        result = ValidationResult()
        result.add_error(ValidationError("Test error", severity="error"))

        with pytest.raises(ValidationError) as exc_info:
            result.raise_if_errors()

        assert "Validation failed" in str(exc_info.value)
        assert "Test error" in str(exc_info.value)

    def test_raise_if_errors_no_error(self):
        """Test that no exception is raised when there are no errors."""
        result = ValidationResult()
        result.add_error(ValidationError("Warning", severity="warning"))
        result.raise_if_errors()  # Should not raise


class TestRequiredFieldValidator:
    """Test RequiredFieldValidator class."""

    def test_required_field_present(self):
        """Test validation passes when required field is present."""
        deck = Deck()
        curve = DefineCurve()
        curve.lcid = 1
        deck.append(curve)

        validator = RequiredFieldValidator("DEFINE_CURVE", "lcid")
        result = ValidationResult()
        validator.validate(deck, result)

        assert result.is_valid
        assert not result.has_errors()

    def test_required_field_missing(self):
        """Test validation fails when required field is None."""
        deck = Deck()
        curve = DefineCurve()
        # lcid is None by default
        deck.append(curve)

        validator = RequiredFieldValidator("DEFINE_CURVE", "lcid")
        result = ValidationResult()
        validator.validate(deck, result)

        assert not result.is_valid
        assert result.has_errors()
        assert len(result.errors) == 1
        assert isinstance(result.errors[0], RequiredFieldError)
        assert "lcid" in str(result.errors[0])

    def test_required_field_pattern_matching(self):
        """Test that pattern matching works for keyword types."""
        deck = Deck()
        # Add multiple DEFINE_CURVE variants
        curve1 = DefineCurve()
        curve1.lcid = 1
        deck.append(curve1)

        curve2 = DefineCurve()  # lcid is None
        deck.append(curve2)

        validator = RequiredFieldValidator("DEFINE_CURVE", "lcid")
        result = ValidationResult()
        validator.validate(deck, result)

        assert not result.is_valid
        assert len(result.errors) == 1

    def test_required_field_warning_severity(self):
        """Test required field validator with warning severity."""
        deck = Deck()
        curve = DefineCurve()
        deck.append(curve)

        validator = RequiredFieldValidator("DEFINE_CURVE", "lcid", ValidationSeverity.WARNING)
        result = ValidationResult()
        validator.validate(deck, result)

        assert result.is_valid  # Warnings don't affect validity
        assert result.has_warnings()
        assert len(result.warnings) == 1


class TestUniqueIDValidator:
    """Test UniqueIDValidator class."""

    def test_unique_ids(self):
        """Test validation passes when IDs are unique."""
        deck = Deck()
        section1 = SectionShell()
        section1.secid = 1
        section2 = SectionShell()
        section2.secid = 2
        deck.append(section1)
        deck.append(section2)

        validator = UniqueIDValidator("SECTION", "secid")
        result = ValidationResult()
        validator.validate(deck, result)

        assert result.is_valid
        assert not result.has_errors()

    def test_duplicate_ids(self):
        """Test validation fails when IDs are duplicated."""
        deck = Deck()
        section1 = SectionShell()
        section1.secid = 1
        section2 = SectionShell()
        section2.secid = 1  # Duplicate
        section3 = SectionShell()
        section3.secid = 2
        deck.append(section1)
        deck.append(section2)
        deck.append(section3)

        validator = UniqueIDValidator("SECTION", "secid")
        result = ValidationResult()
        validator.validate(deck, result)

        assert not result.is_valid
        assert result.has_errors()
        assert len(result.errors) == 1
        assert isinstance(result.errors[0], DuplicateIDError)
        assert "1" in str(result.errors[0])

    def test_none_ids_ignored(self):
        """Test that None values are ignored in uniqueness check."""
        deck = Deck()
        section1 = SectionShell()
        section1.secid = None
        section2 = SectionShell()
        section2.secid = None
        section3 = SectionShell()
        section3.secid = 1
        deck.append(section1)
        deck.append(section2)
        deck.append(section3)

        validator = UniqueIDValidator("SECTION", "secid")
        result = ValidationResult()
        validator.validate(deck, result)

        assert result.is_valid  # None values should not be considered duplicates


class TestCustomValidator:
    """Test CustomValidator class."""

    def test_custom_validator(self):
        """Test custom validation function."""

        def check_curve_count(deck, result):
            curves = deck["DEFINE_CURVE"]  # Use deck indexer
            curve_count = len(curves)
            if curve_count < 2:
                error = ValidationError(f"Deck must have at least 2 curves, found {curve_count}", severity="error")
                result.add_error(error)

        deck = Deck()
        curve = DefineCurve()
        curve.lcid = 1
        deck.append(curve)

        validator = CustomValidator("MinCurveCount", check_curve_count)
        result = ValidationResult()
        validator.validate(deck, result)

        assert not result.is_valid
        assert result.has_errors()
        assert "at least 2 curves" in str(result.errors[0])


class TestValidatorRegistry:
    """Test ValidatorRegistry class."""

    def test_register_and_validate(self):
        """Test registering validators and running validation."""
        registry = ValidatorRegistry()
        registry.register(RequiredFieldValidator("DEFINE_CURVE", "lcid"))

        deck = Deck()
        curve = DefineCurve()
        deck.append(curve)

        result = registry.validate(deck)

        assert not result.is_valid
        assert result.has_errors()

    def test_register_custom(self):
        """Test registering custom validation function."""
        registry = ValidatorRegistry()

        def custom_check(deck, result):
            error = ValidationError("Custom error", severity="error")
            result.add_error(error)

        registry.register_custom("CustomCheck", custom_check)

        deck = Deck()
        result = registry.validate(deck)

        assert not result.is_valid
        assert "Custom error" in str(result.errors[0])

    def test_unregister(self):
        """Test unregistering validators."""
        registry = ValidatorRegistry()
        validator = RequiredFieldValidator("DEFINE_CURVE", "lcid")
        registry.register(validator)

        assert len(registry.get_all()) == 1

        removed = registry.unregister("RequiredField[DEFINE_CURVE.lcid]")
        assert removed
        assert len(registry.get_all()) == 0

        removed = registry.unregister("NonExistent")
        assert not removed

    def test_clear(self):
        """Test clearing all validators."""
        registry = ValidatorRegistry()
        registry.register(RequiredFieldValidator("DEFINE_CURVE", "lcid"))
        registry.register(UniqueIDValidator("SECTION", "secid"))

        assert len(registry.get_all()) == 2

        registry.clear()
        assert len(registry.get_all()) == 0

    def test_register_default_validators(self):
        """Test registering default validators."""
        registry = ValidatorRegistry()
        registry.register_default_validators()

        validators = registry.get_all()
        assert len(validators) > 0

        # Check that defaults include required validators
        validator_names = [v.get_name() for v in validators]
        assert any("DEFINE_CURVE.lcid" in name for name in validator_names)
        assert any("SECTION.secid" in name for name in validator_names)

    def test_register_default_validators_idempotent(self):
        """Test that registering defaults multiple times doesn't duplicate."""
        registry = ValidatorRegistry()
        registry.register_default_validators()
        count1 = len(registry.get_all())

        registry.register_default_validators()
        count2 = len(registry.get_all())

        assert count1 == count2

    def test_validator_exception_handling(self):
        """Test that exceptions in validators are caught and reported."""
        registry = ValidatorRegistry()

        def failing_validator(deck, result):
            raise RuntimeError("Validator exploded")

        registry.register_custom("FailingValidator", failing_validator)

        deck = Deck()
        result = registry.validate(deck)

        assert not result.is_valid
        assert result.has_errors()
        assert "FailingValidator" in str(result.errors[0])
        assert "exception" in str(result.errors[0]).lower()


class TestDeckValidation:
    """Test Deck validation methods."""

    def test_validate_with_registry(self):
        """Test deck validation using the registry."""
        deck = Deck()
        curve = DefineCurve()
        deck.append(curve)

        result = deck.validate()

        assert not result.is_valid
        assert result.has_errors()

    def test_validate_detects_duplicates(self):
        """Test that default validators detect duplicate IDs."""
        deck = Deck()
        section1 = SectionShell()
        section1.secid = 1
        section2 = SectionShell()
        section2.secid = 1
        deck.append(section1)
        deck.append(section2)

        # Validation should detect duplicate secid
        result = deck.validate()
        assert not result.is_valid
        assert result.has_errors()
        assert "duplicate" in str(result.errors[0]).lower()

    def test_register_validator(self):
        """Test registering custom validators on deck."""
        deck = Deck()
        validator = RequiredFieldValidator("MAT", "mid", ValidationSeverity.ERROR)
        deck.register_validator(validator)

        validators = deck.get_validators()
        assert len(validators) > 0

    def test_register_custom_validator(self):
        """Test registering custom validation function on deck."""
        deck = Deck()

        def custom_check(deck, result):
            pass  # No-op for test

        deck.register_custom_validator("CustomCheck", custom_check)

        validators = deck.get_validators()
        validator_names = [v.get_name() for v in validators]
        assert "CustomCheck" in validator_names

    def test_unregister_validator(self):
        """Test unregistering validators from deck."""
        deck = Deck()
        deck.clear_validators()
        validator = RequiredFieldValidator("MAT", "mid")
        deck.register_validator(validator)

        assert len(deck.get_validators()) == 1

        removed = deck.unregister_validator("RequiredField[MAT.mid]")
        assert removed
        assert len(deck.get_validators()) == 0

    def test_clear_validators(self):
        """Test clearing all validators from deck."""
        deck = Deck()
        deck.register_validator(RequiredFieldValidator("MAT", "mid"))
        deck.register_validator(UniqueIDValidator("SECTION", "secid"))

        deck.clear_validators()
        assert len(deck.get_validators()) == 0

    def test_export_with_validation(self):
        """Test exporting with validation enabled."""
        deck = Deck()
        curve = DefineCurve()
        curve.lcid = 1
        deck.append(curve)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test.k"
            deck.export_file(str(path), validate=True)  # Should succeed

            assert path.exists()

    def test_export_with_validation_failure(self):
        """Test that export fails when validation detects errors."""
        deck = Deck()
        curve = DefineCurve()  # lcid is None
        deck.append(curve)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test.k"

            with pytest.raises(ValidationError):
                deck.export_file(str(path), validate=True)

            # File should not be created when validation fails
            # (well, it might be partially created depending on when validation runs)

    def test_write_with_validation(self):
        """Test write with validation enabled."""
        deck = Deck()
        curve = DefineCurve()
        curve.lcid = 1
        deck.append(curve)

        output = deck.write(validate=True)
        assert output is not None
        assert "*DEFINE_CURVE" in output

    def test_write_with_validation_failure(self):
        """Test that write fails when validation detects errors."""
        deck = Deck()
        curve = DefineCurve()  # lcid is None
        deck.append(curve)

        with pytest.raises(ValidationError):
            deck.write(validate=True)

    def test_write_without_validation(self):
        """Test that write succeeds without validation even when deck is invalid."""
        deck = Deck()
        curve = DefineCurve()  # lcid is None
        deck.append(curve)

        # Should not raise even though deck is invalid
        output = deck.write(validate=False)
        assert output is not None


class TestValidationIntegration:
    """Integration tests for validation system."""

    def test_multiple_validators(self):
        """Test running multiple validators together."""
        deck = Deck()

        # Invalid curve (no lcid)
        curve = DefineCurve()
        deck.append(curve)

        # Duplicate sections
        section1 = SectionShell()
        section1.secid = 1
        section2 = SectionShell()
        section2.secid = 1
        deck.append(section1)
        deck.append(section2)

        result = deck.validate()

        # Should have errors from both validators
        assert not result.is_valid
        assert len(result.errors) >= 2

    def test_severity_levels(self):
        """Test different severity levels."""
        deck = Deck()
        deck.clear_validators()

        # Add warning-level validator
        deck.register_validator(RequiredFieldValidator("DEFINE_CURVE", "lcid", ValidationSeverity.WARNING))

        curve = DefineCurve()
        deck.append(curve)

        result = deck.validate()

        # Should be valid (only warnings)
        assert result.is_valid
        assert result.has_warnings()
        assert not result.has_errors()

    def test_mixed_severity(self):
        """Test mix of errors and warnings."""
        deck = Deck()
        deck.clear_validators()

        # Error-level validator
        deck.register_validator(RequiredFieldValidator("DEFINE_CURVE", "lcid", ValidationSeverity.ERROR))

        # Warning-level validator
        deck.register_validator(UniqueIDValidator("SECTION", "secid", ValidationSeverity.WARNING))

        # Invalid curve (error)
        curve = DefineCurve()
        deck.append(curve)

        # Duplicate sections (warning)
        section1 = SectionShell()
        section1.secid = 1
        section2 = SectionShell()
        section2.secid = 1
        deck.append(section1)
        deck.append(section2)

        result = deck.validate()

        assert not result.is_valid
        assert result.has_errors()
        assert result.has_warnings()

    def test_custom_validation_workflow(self):
        """Test a realistic custom validation workflow."""
        deck = Deck()
        deck.clear_validators()

        # Custom validator: check that all sections have corresponding materials
        def check_material_references(deck, result):
            section_mids = set()
            for section in deck.get_kwds_by_type("SECTION"):
                if hasattr(section, "elform"):
                    # Sections might reference materials (simplified example)
                    section_mids.add(1)  # Dummy example

            material_mids = set()
            for mat in deck.get_kwds_by_type("MAT"):
                if hasattr(mat, "mid"):
                    mid = getattr(mat, "mid")
                    if mid is not None:
                        material_mids.add(mid)

            missing = section_mids - material_mids
            if missing:
                error = ValidationError(f"Sections reference undefined materials: {missing}", severity="warning")
                result.add_error(error)

        deck.register_custom_validator("MaterialReferences", check_material_references)

        # Add section but no material
        section = SectionShell()
        section.secid = 1
        deck.append(section)

        result = deck.validate()

        # Should have warning about missing material
        assert result.is_valid  # Warnings don't fail validation
        assert result.has_warnings()


class TestGloballyUniqueKeywordValidator:
    """Test GloballyUniqueKeywordValidator class."""

    def test_single_instance_passes(self):
        """Test validation passes when globally unique keyword appears once."""
        deck = Deck()
        deck.clear_validators()

        timestep = ControlTimestep()
        deck.append(timestep)

        validator = GloballyUniqueKeywordValidator()
        result = ValidationResult()
        validator.validate(deck, result)

        assert result.is_valid
        assert not result.has_errors()

    def test_duplicate_keyword_fails(self):
        """Test validation fails when globally unique keyword appears multiple times."""
        deck = Deck()
        deck.clear_validators()

        timestep1 = ControlTimestep()
        timestep2 = ControlTimestep()
        deck.append(timestep1)
        deck.append(timestep2)

        validator = GloballyUniqueKeywordValidator()
        result = ValidationResult()
        validator.validate(deck, result)

        assert not result.is_valid
        assert result.has_errors()
        assert len(result.errors) == 1
        assert isinstance(result.errors[0], DuplicateKeywordError)
        assert "CONTROL_TIMESTEP" in str(result.errors[0])
        assert "2 times" in str(result.errors[0])

    def test_multiple_duplicates(self):
        """Test detection of multiple different duplicate keywords."""
        deck = Deck()
        deck.clear_validators()

        # Add 3 CONTROL_TIMESTEP keywords
        deck.append(ControlTimestep())
        deck.append(ControlTimestep())
        deck.append(ControlTimestep())

        validator = GloballyUniqueKeywordValidator()
        result = ValidationResult()
        validator.validate(deck, result)

        assert not result.is_valid
        assert len(result.errors) == 1
        error = result.errors[0]
        assert error.count == 3

    def test_non_unique_keywords_ignored(self):
        """Test that non-globally-unique keywords are not flagged."""
        deck = Deck()
        deck.clear_validators()

        # Multiple SECTION_SHELL keywords are allowed
        section1 = SectionShell()
        section1.secid = 1
        section2 = SectionShell()
        section2.secid = 2
        deck.append(section1)
        deck.append(section2)

        validator = GloballyUniqueKeywordValidator()
        result = ValidationResult()
        validator.validate(deck, result)

        assert result.is_valid
        assert not result.has_errors()

    def test_warning_severity(self):
        """Test globally unique keyword validator with warning severity."""
        deck = Deck()
        deck.clear_validators()

        deck.append(ControlTimestep())
        deck.append(ControlTimestep())

        validator = GloballyUniqueKeywordValidator(ValidationSeverity.WARNING)
        result = ValidationResult()
        validator.validate(deck, result)

        assert result.is_valid  # Warnings don't affect validity
        assert result.has_warnings()
        assert len(result.warnings) == 1

    def test_default_validators_include_globally_unique(self):
        """Test that default validators include globally unique keyword check."""
        deck = Deck()

        deck.append(ControlTimestep())
        deck.append(ControlTimestep())

        result = deck.validate()

        assert not result.is_valid
        # Should have error about duplicate CONTROL_TIMESTEP
        found_duplicate_error = any(
            isinstance(e, DuplicateKeywordError) for e in result.errors
        )
        assert found_duplicate_error
