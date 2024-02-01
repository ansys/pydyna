"""Module for the trame visualizer."""
try:
    from pyvista.trame.ui import plotter_ui
    from trame.app import get_server
    from trame.ui.vuetify import SinglePageLayout

    _HAS_TRAME = True

except ModuleNotFoundError:  # pragma: no cover
    _HAS_TRAME = False


class TrameVisualizer:
    """Defines the view layout for the Trame visualizer."""

    def __init__(self) -> None:
        """Initialize the server and server-related variables."""
        if not _HAS_TRAME:  # pragma: no cover
            raise ModuleNotFoundError(
                "The package 'pyvista[trame]' is required to use this function."
            )

        self.server = get_server()
        self.state, self.ctrl = self.server.state, self.server.controller

    def set_scene(self, plotter):
        """Set the view layout for the Trame visualizer.

        This method also sets the mesh to show by the PyVista plotter.

        Parameters
        ----------
        plotter : pv.Plotter
            PyVista plotter to render the mesh.
        """
        self.state.trame__title = "PyPrime Viewer"

        with SinglePageLayout(self.server) as layout:
            layout.icon.click = self.ctrl.view_reset_camera
            layout.title.set_text("PyPrime")

            with layout.content:
                # Use PyVista UI template for Plotters
                view = plotter_ui(plotter)
                self.ctrl.view_update = view.update

            # hide footer with trame watermark
            layout.footer.hide()

    def show(self):
        """Start the server and show the mesh."""
        self.server.start()
