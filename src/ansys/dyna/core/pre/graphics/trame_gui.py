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

"""Module for the trame visualizer."""

try:
    from pyvista.trame.ui import plotter_ui
    from trame.app import get_server
    from trame.ui.vuetify3 import SinglePageLayout
    from trame.widgets import vuetify3

    _HAS_TRAME = True

except ModuleNotFoundError:  # pragma: no cover
    _HAS_TRAME = False


class TrameVisualizer:
    """Defines the view layout for the Trame visualizer."""

    def __init__(self) -> None:
        """Initialize the server and server-related variables."""
        if not _HAS_TRAME:  # pragma: no cover
            raise ModuleNotFoundError("The package 'pyvista[trame]' is required to use this function.")

        self.server = get_server(client_type="vue3")
        self.state, self.ctrl = self.server.state, self.server.controller

    def set_scene(self, plotter):
        """Set the view layout for the Trame visualizer.

        This method also sets the mesh to show by the PyVista plotter.

        Parameters
        ----------
        plotter : pv.Plotter
            PyVista plotter to render the mesh.
        """
        self.state.trame__title = "PyDYNA Viewer"

        with SinglePageLayout(self.server) as layout:
            layout.icon.click = self.ctrl.view_reset_camera
            layout.title.set_text("PyDYNA")

            with layout.content:
                with vuetify3.VContainer(fluid=True, classes="pa-0 fill-height"):
                    # Use PyVista UI template for Plotters
                    view = plotter_ui(plotter)
                    self.ctrl.view_update = view.update

            # hide footer with trame watermark
            layout.footer.hide()

    def show(self):
        """Start the server and show the mesh."""
        self.server.start()
