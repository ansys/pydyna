"""Tests for BaseRunner.get_memory_string."""

import pytest

from ansys.dyna.core.run.base_runner import BaseRunner
from ansys.dyna.core.run.options import MemoryUnit


@pytest.mark.parametrize(
    "memory_unit, expected",
    [(MemoryUnit.MB, "20m"), (MemoryUnit.GB, "20G")],
)
def test_memory_string_supported(memory_unit, expected):
    assert BaseRunner(memory=20, memory_unit=memory_unit).get_memory_string() == expected


@pytest.mark.parametrize(
    "memory_unit",
    [MemoryUnit.BYTE, MemoryUnit.KB, MemoryUnit.TB],
)
def test_memory_string_unsupported_raises_value_error(memory_unit):
    runner = BaseRunner(memory=20, memory_unit=memory_unit)
    with pytest.raises(ValueError, match="Only MemoryUnit.MB and MemoryUnit.GB are supported"):
        runner.get_memory_string()
        
        assert str(memory_unit) in str(excinfo.value)


def test_memory_string_default_is_mb():
    assert BaseRunner(memory=5).get_memory_string() == "5m"
