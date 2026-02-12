# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT

"""Tests for CurvePlottingMixin."""

from unittest.mock import MagicMock, Mock, patch

import numpy as np
import pandas as pd
import pytest

from ansys.dyna.core.lib.mixins.curve_plotting import CurvePlottingMixin


class FakeCurveKeyword(CurvePlottingMixin):
    """Fake keyword class for testing the mixin in isolation."""

    def __init__(self):
        self.curves = pd.DataFrame()
        self.lcid = 1
        self.title = "Test Curve"
        self.sfa = 1.0
        self.sfo = 1.0
        self.offa = 0.0
        self.offo = 0.0


class TestCurvePlottingMixin:
    """Test suite for CurvePlottingMixin."""

    def test_get_curve_data_basic(self):
        """Test basic curve data extraction without transforms."""
        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0, 1.0, 2.0], "o1": [0.0, 2.0, 4.0]})

        x, y = fake_curve.get_curve_data(apply_transforms=False)

        assert len(x) == 3
        assert len(y) == 3
        np.testing.assert_array_equal(x, [0.0, 1.0, 2.0])
        np.testing.assert_array_equal(y, [0.0, 2.0, 4.0])

    def test_get_curve_data_with_transforms(self):
        """Test curve data extraction with scale factors and offsets."""
        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0, 1.0, 2.0], "o1": [0.0, 2.0, 4.0]})
        fake_curve.sfa = 2.0  # Double x values
        fake_curve.sfo = 0.5  # Halve y values
        fake_curve.offa = 1.0  # Add 1 to x
        fake_curve.offo = 3.0  # Add 3 to y

        x, y = fake_curve.get_curve_data(apply_transforms=True)

        # Expected: x = 2.0 * [0, 1, 2] + 1.0 = [1.0, 3.0, 5.0]
        # Expected: y = 0.5 * [0, 2, 4] + 3.0 = [3.0, 4.0, 5.0]
        np.testing.assert_array_almost_equal(x, [1.0, 3.0, 5.0])
        np.testing.assert_array_almost_equal(y, [3.0, 4.0, 5.0])

    def test_get_curve_data_empty_raises_error(self):
        """Test that empty curve data raises ValueError."""
        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame()

        with pytest.raises(ValueError, match="Curve data is empty"):
            fake_curve.get_curve_data()

    def test_get_curve_data_missing_columns_raises_error(self):
        """Test that missing columns raise ValueError."""
        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"x": [1, 2], "y": [3, 4]})

        with pytest.raises(ValueError, match="must have 'a1'.*and 'o1'"):
            fake_curve.get_curve_data()

    def test_get_curve_data_no_curves_attribute_raises_error(self):
        """Test that missing curves attribute raises AttributeError."""

        class FakeWithoutCurves(CurvePlottingMixin):
            pass

        fake = FakeWithoutCurves()

        with pytest.raises(AttributeError, match="does not have curve data"):
            fake.get_curve_data()

    def test_get_curve_data_none_transform_values(self):
        """Test that None values for transforms are treated as defaults."""
        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [1.0, 2.0], "o1": [3.0, 4.0]})
        fake_curve.sfa = None  # Should default to 1.0
        fake_curve.sfo = None  # Should default to 1.0
        fake_curve.offa = None  # Should default to 0.0
        fake_curve.offo = None  # Should default to 0.0

        x, y = fake_curve.get_curve_data(apply_transforms=True)

        # Should be same as raw data since defaults are identity transforms
        np.testing.assert_array_equal(x, [1.0, 2.0])
        np.testing.assert_array_equal(y, [3.0, 4.0])

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_basic(self, mock_get_matplotlib):
        """Test basic plotting without showing the plot."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_fig = Mock()
        mock_plt.subplots.return_value = (mock_fig, mock_ax)
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0, 1.0, 2.0], "o1": [0.0, 1.0, 4.0]})

        result = fake_curve.plot(show=False)

        # Verify plot was called with correct data
        mock_ax.plot.assert_called_once()
        call_args = mock_ax.plot.call_args
        x_data, y_data = call_args[0]
        np.testing.assert_array_equal(x_data, [0.0, 1.0, 2.0])
        np.testing.assert_array_equal(y_data, [0.0, 1.0, 4.0])

        # Verify labels and title were set
        mock_ax.set_title.assert_called_once()
        mock_ax.set_xlabel.assert_called_once()
        mock_ax.set_ylabel.assert_called_once()
        mock_ax.grid.assert_called_once()

        # Verify show was not called
        mock_plt.show.assert_not_called()

        # Verify return value
        assert result == mock_ax

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_with_custom_labels(self, mock_get_matplotlib):
        """Test plotting with custom labels and title."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_fig = Mock()
        mock_plt.subplots.return_value = (mock_fig, mock_ax)
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0, 1.0], "o1": [0.0, 1.0]})

        fake_curve.plot(
            show=False, title="Custom Title", xlabel="Time (s)", ylabel="Force (N)", color="red", linewidth=2
        )

        mock_ax.set_title.assert_called_with("Custom Title")
        mock_ax.set_xlabel.assert_called_with("Time (s)")
        mock_ax.set_ylabel.assert_called_with("Force (N)")

        # Verify kwargs were passed through
        call_kwargs = mock_ax.plot.call_args[1]
        assert call_kwargs["color"] == "red"
        assert call_kwargs["linewidth"] == 2

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_with_transforms(self, mock_get_matplotlib):
        """Test plotting with scale factors and offsets."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_fig = Mock()
        mock_plt.subplots.return_value = (mock_fig, mock_ax)
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0, 1.0, 2.0], "o1": [0.0, 1.0, 4.0]})
        fake_curve.sfa = 2.0
        fake_curve.sfo = 3.0
        fake_curve.offa = 1.0
        fake_curve.offo = 2.0

        fake_curve.plot(show=False, apply_transforms=True)

        # Get the plotted data
        call_args = mock_ax.plot.call_args[0]
        x_data, y_data = call_args

        # Expected: x = 2.0 * [0, 1, 2] + 1.0 = [1.0, 3.0, 5.0]
        # Expected: y = 3.0 * [0, 1, 4] + 2.0 = [2.0, 5.0, 14.0]
        np.testing.assert_array_almost_equal(x_data, [1.0, 3.0, 5.0])
        np.testing.assert_array_almost_equal(y_data, [2.0, 5.0, 14.0])

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_with_existing_axes(self, mock_get_matplotlib):
        """Test plotting on existing matplotlib axes."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0, 1.0], "o1": [0.0, 1.0]})

        result = fake_curve.plot(show=False, ax=mock_ax)

        # Verify subplots was not called (we provided an axes)
        mock_plt.subplots.assert_not_called()

        # Verify plotting happened on provided axes
        mock_ax.plot.assert_called_once()

        # Verify return value is the provided axes
        assert result == mock_ax

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_with_show(self, mock_get_matplotlib):
        """Test that show=True calls plt.show()."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_fig = Mock()
        mock_plt.subplots.return_value = (mock_fig, mock_ax)
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0, 1.0], "o1": [0.0, 1.0]})

        fake_curve.plot(show=True)

        mock_plt.show.assert_called_once()

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_empty_raises_error(self, mock_get_matplotlib):
        """Test that plotting empty curve data raises ValueError."""
        mock_plt = Mock()
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame()

        with pytest.raises(ValueError, match="Curve data is empty"):
            fake_curve.plot()

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_title_with_lcid_and_title(self, mock_get_matplotlib):
        """Test automatic title generation with lcid and title."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_fig = Mock()
        mock_plt.subplots.return_value = (mock_fig, mock_ax)
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0], "o1": [0.0]})
        fake_curve.lcid = 42
        fake_curve.title = "My Test Curve"

        fake_curve.plot(show=False)

        mock_ax.set_title.assert_called_with("Curve 42: My Test Curve")

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_title_with_lcid_only(self, mock_get_matplotlib):
        """Test automatic title generation with only lcid."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_fig = Mock()
        mock_plt.subplots.return_value = (mock_fig, mock_ax)
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0], "o1": [0.0]})
        fake_curve.lcid = 42
        fake_curve.title = None

        fake_curve.plot(show=False)

        mock_ax.set_title.assert_called_with("Curve 42")

    @patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib")
    def test_plot_title_default(self, mock_get_matplotlib):
        """Test automatic title generation with no lcid or title."""
        mock_plt = Mock()
        mock_ax = Mock()
        mock_fig = Mock()
        mock_plt.subplots.return_value = (mock_fig, mock_ax)
        mock_get_matplotlib.return_value = mock_plt

        fake_curve = FakeCurveKeyword()
        fake_curve.curves = pd.DataFrame({"a1": [0.0], "o1": [0.0]})
        fake_curve.lcid = None
        fake_curve.title = None

        fake_curve.plot(show=False)

        mock_ax.set_title.assert_called_with("Curve")

    def test_matplotlib_import_error(self):
        """Test that missing matplotlib raises helpful ImportError."""
        with patch("ansys.dyna.core.lib.mixins.curve_plotting._get_matplotlib") as mock_get:
            mock_get.side_effect = ImportError("test error")

            fake_curve = FakeCurveKeyword()
            fake_curve.curves = pd.DataFrame({"a1": [0.0], "o1": [0.0]})

            with pytest.raises(ImportError):
                fake_curve.plot()
