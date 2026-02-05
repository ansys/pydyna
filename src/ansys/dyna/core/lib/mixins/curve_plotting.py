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

"""Mixin providing curve plotting functionality for DefineCurve classes."""

import logging
import typing

import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)


def _get_matplotlib():
    """Lazy import matplotlib to avoid slow startup times."""
    try:
        import matplotlib.pyplot as plt

        return plt
    except ImportError:
        raise ImportError(
            "Matplotlib is required for plotting. Install it with: pip install ansys-dyna-core[graphics]"
        )


class CurvePlottingMixin:
    """Mixin that provides plotting capabilities for DefineCurve classes.

    This mixin adds a `.plot()` method to curve classes, enabling 2D curve
    visualization using matplotlib. The method automatically applies scale
    factors and offsets (sfa, sfo, offa, offo) to the curve data.

    Attributes expected from the host class:
        curves: pd.DataFrame with 'a1' (abscissa) and 'o1' (ordinate) columns
        sfa: float - scale factor for abscissa
        sfo: float - scale factor for ordinate
        offa: float - offset for abscissa
        offo: float - offset for ordinate
        lcid: int - load curve ID (for plot title)
        title: str - optional title text
    """

    def plot(
        self,
        apply_transforms: bool = True,
        title: typing.Optional[str] = None,
        xlabel: typing.Optional[str] = None,
        ylabel: typing.Optional[str] = None,
        show: bool = True,
        ax: typing.Optional[typing.Any] = None,
        **kwargs,
    ) -> typing.Any:
        """Plot the curve using matplotlib.

        Parameters
        ----------
        apply_transforms : bool, default=True
            If True, apply scale factors (sfa, sfo) and offsets (offa, offo) to the curve data.
            If False, plot raw data from the curves table.
        title : str, optional
            Plot title. If not provided, uses the curve's lcid and title attribute.
        xlabel : str, optional
            X-axis label. Defaults to "Abscissa" (or "Time" if transforms applied).
        ylabel : str, optional
            Y-axis label. Defaults to "Ordinate" (or "Value" if transforms applied).
        show : bool, default=True
            If True, display the plot immediately using plt.show().
            If False, return the axes object for further customization.
        ax : matplotlib.axes.Axes, optional
            Matplotlib axes to plot on. If None, creates a new figure and axes.
        **kwargs
            Additional keyword arguments passed to matplotlib's plot() function
            (e.g., color, linewidth, linestyle, marker, etc.)

        Returns
        -------
        matplotlib.axes.Axes
            The axes object containing the plot. Can be used for further customization.

        Raises
        ------
        ImportError
            If matplotlib is not installed.
        ValueError
            If the curve data is empty or invalid.

        Examples
        --------
        >>> curve = DefineCurve(lcid=1)
        >>> curve.curves = pd.DataFrame({"a1": [0, 1, 2], "o1": [0, 1, 4]})
        >>> curve.plot()  # Plot with default settings

        >>> curve.plot(apply_transforms=False, color='red', linewidth=2)  # Customize plot

        >>> fig, ax = plt.subplots()
        >>> curve.plot(ax=ax, show=False)  # Plot on existing axes
        >>> ax.grid(True)
        >>> plt.show()
        """
        plt = _get_matplotlib()

        # Get curve data
        if not hasattr(self, "curves"):
            raise AttributeError("This keyword does not have curve data (no 'curves' attribute)")

        curves_df: pd.DataFrame = self.curves

        if curves_df.empty:
            raise ValueError("Curve data is empty. Add data to the 'curves' DataFrame before plotting.")

        # Extract abscissa and ordinate columns
        if "a1" not in curves_df.columns or "o1" not in curves_df.columns:
            raise ValueError("Curve data must have 'a1' (abscissa) and 'o1' (ordinate) columns")

        x_data = curves_df["a1"].values
        y_data = curves_df["o1"].values

        # Apply transformations if requested
        if apply_transforms:
            sfa = getattr(self, "sfa", 1.0) or 1.0
            sfo = getattr(self, "sfo", 1.0) or 1.0
            offa = getattr(self, "offa", 0.0) or 0.0
            offo = getattr(self, "offo", 0.0) or 0.0

            x_data = sfa * x_data + offa
            y_data = sfo * y_data + offo

        # Create axes if not provided
        if ax is None:
            fig, ax = plt.subplots()

        # Plot the curve
        ax.plot(x_data, y_data, **kwargs)

        # Set title
        if title is None:
            lcid = getattr(self, "lcid", None)
            kwd_title = getattr(self, "title", None)
            if kwd_title:
                title = f"Curve {lcid}: {kwd_title}"
            elif lcid is not None:
                title = f"Curve {lcid}"
            else:
                title = "Curve"
        ax.set_title(title)

        # Set axis labels
        if xlabel is None:
            xlabel = "Abscissa" if not apply_transforms else "Time"
        if ylabel is None:
            ylabel = "Ordinate" if not apply_transforms else "Value"

        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)

        # Add grid for better readability
        ax.grid(True, alpha=0.3)

        # Show plot if requested
        if show:
            plt.show()

        return ax

    def get_curve_data(
        self,
        apply_transforms: bool = True,
    ) -> typing.Tuple[np.ndarray, np.ndarray]:
        """Get the curve data as numpy arrays.

        Parameters
        ----------
        apply_transforms : bool, default=True
            If True, apply scale factors (sfa, sfo) and offsets (offa, offo) to the curve data.
            If False, return raw data from the curves table.

        Returns
        -------
        tuple of (x_data, y_data)
            Two numpy arrays containing the abscissa and ordinate values.

        Raises
        ------
        AttributeError
            If the keyword does not have curve data.
        ValueError
            If the curve data is empty or invalid.

        Examples
        --------
        >>> curve = DefineCurve(lcid=1)
        >>> curve.curves = pd.DataFrame({"a1": [0, 1, 2], "o1": [0, 1, 4]})
        >>> x, y = curve.get_curve_data()
        >>> print(x)  # [0. 1. 2.]
        >>> print(y)  # [0. 1. 4.]
        """
        if not hasattr(self, "curves"):
            raise AttributeError("This keyword does not have curve data (no 'curves' attribute)")

        curves_df: pd.DataFrame = self.curves

        if curves_df.empty:
            raise ValueError("Curve data is empty.")

        if "a1" not in curves_df.columns or "o1" not in curves_df.columns:
            raise ValueError("Curve data must have 'a1' (abscissa) and 'o1' (ordinate) columns")

        x_data = curves_df["a1"].values
        y_data = curves_df["o1"].values

        # Apply transformations if requested
        if apply_transforms:
            sfa = getattr(self, "sfa", 1.0) or 1.0
            sfo = getattr(self, "sfo", 1.0) or 1.0
            offa = getattr(self, "offa", 0.0) or 0.0
            offo = getattr(self, "offo", 0.0) or 0.0

            x_data = sfa * x_data + offa
            y_data = sfo * y_data + offo

        return x_data, y_data
