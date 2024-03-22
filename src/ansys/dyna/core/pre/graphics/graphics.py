"""Module for graphics-related implementations."""
import enum
import os
from typing import List

import numpy as np
import pyvista as pv
from pyvista.plotting import Plotter
import vtk

import ansys.dyna.core.pre as pre
from ansys.dyna.core.pre.graphics.trame_gui import _HAS_TRAME, TrameVisualizer
from ansys.dyna.core.pre.internals import defaults


class DisplayMeshType(enum.IntEnum):
    """Contains the mesh types to display."""

    FACE = 0
    BEAM = 1
    CYLINDER = 2
    SPHERE = 3
    PLANAR = 4


class ColorByType(enum.IntEnum):
    """Contains the zone types to display."""

    ZONE = 0
    ZONELET = 1
    PART = 2


""" bright color palette """
"""color_matrix = np.array([
[0.99609375, 0.76171875, 0.0703125],
[0.96484375, 0.62109375, 0.12109375],
[0.9296875, 0.3515625, 0.140625],
[0.9140625, 0.125, 0.15234375],
[0.765625, 0.89453125, 0.21875],
[0.63671875, 0.79296875, 0.21875],
[0, 0.578125, 0.1953125],
[0, 0.3828125, 0.3984375],
[0.0703125, 0.79296875, 0.765625],
[0.0703125, 0.53515625, 0.65234375],
[0.0234375, 0.3203125, 0.86328125],
[0.10546875, 0.078125, 0.390625],
[0.98828125, 0.65234375, 0.87109375],
[0.84765625, 0.5, 0.9765625],
[0.59765625, 0.5, 0.9765625] ]) * 255 """


""" winter color palette """
"""color_matrix = np.array([
[0.2734375, 0.71875, 0.8671875],
[0.2734375, 0.90234375, 0.86328125],
[0.2734375, 0.87109375, 0.62109375],
[0.2734375, 0.80078125, 0.84765625],
[0.2734375, 0.70703125, 0.66796875],
[0.2734375, 0.84375, 0.90234375],
[0.2734375, 0.64453125, 0.71875],
[0.2734375, 0.921875, 0.703125],
[0.2734375, 0.765625, 0.66015625],
[0.2734375, 0.9140625, 0.87890625],
[0.2734375, 0.70703125, 0.5],
[0.2734375, 0.8828125, 0.70703125],
[0.2734375, 0.99609375, 0.8515625],
[0.2734375, 0.91015625, 0.9765625],
[0.2734375, 0.99609375, 0.6171875] ]) * 255"""

""" light fall color palette """
color_matrix = np.array(
    [
        [155, 186, 126],
        [242, 236, 175],
        [255, 187, 131],
        [194, 187, 97],
        [159, 131, 169],
        [157, 190, 139],
        [233, 218, 158],
        [254, 252, 196],
        [246, 210, 148],
        [215, 208, 198],
        [196, 235, 145],
    ]
)


def compute_distance(point1, point2):
    """Compute the distance between two points."""
    dist = np.linalg.norm(np.array(point2) - np.array(point1))
    return dist


class Picker:
    """Contains the items that can be selected from the display with the mouse.

    Parameters
    ----------
    plotter : Plotter
        PyVista plotter to manipulate.
    graphics : Graphics
        Class for providing callbacks.
    """

    def __init__(self, plotter: pv.Plotter, graphics):
        """Initialize the picker."""
        self.plotter = plotter
        self._graphics = graphics
        self._selected_disp_mesh: list[_DisplayMesh] = []
        self._points = []
        self._ignore = False

    @property
    def selections(self):
        """All selected meshes in the display."""
        return self._selected_disp_mesh

    def clear_selection(self):
        """Clear all picked selections in the display."""
        [disp_mesh.deselect() for disp_mesh in self._selected_disp_mesh]
        self._selected_disp_mesh.clear()

    def ignore(self, ignore_pick):
        """Setter for ignore_pick.

        Parameters
        ----------
        ignore_pick : Any
            Picked selection to ignore.
        """
        self._ignore = ignore_pick

    def __call__(self, *args, **kwargs):  # pragma: no cover
        """Call the code to run when something is clicked in the display."""
        if self._ignore:
            return
        picked_pt = np.array(self.plotter.pick_mouse_position())
        direction = picked_pt - self.plotter.camera_position[0]
        direction = direction / np.linalg.norm(direction)
        start = picked_pt - 1000 * direction
        end = picked_pt + 10000 * direction
        disp_mesh_list = self._graphics.get_face_mesh_data()
        closest_disp_mesh = None
        closest_dist = np.finfo(0.0).max
        for disp_mesh in disp_mesh_list:
            if disp_mesh.poly_data is None:
                continue
            point, ix = disp_mesh.poly_data.ray_trace(start, end, first_point=True)
            if ix.size != 0:
                dist = compute_distance(start, point)
                if dist < closest_dist and disp_mesh not in self._selected_disp_mesh:
                    closest_dist = dist
                    closest_disp_mesh = disp_mesh
                elif disp_mesh in self._selected_disp_mesh:
                    disp_mesh.deselect()
                    self._selected_disp_mesh.remove(disp_mesh)
        if closest_disp_mesh is not None:
            self._selected_disp_mesh.append(closest_disp_mesh)
        [disp_mesh.select() for disp_mesh in self._selected_disp_mesh]
        return


class Graphics(object):
    """Manages graphics in PyDYNA.

    Parameters
    ----------
    model : pydyna.pre.Model
        Model to show.
    use_trame : bool, optional
        Whether to use the Trame visualizer. The default is ``False``.
    """

    def __init__(self, model: pre.Model, use_trame: bool = False, view_position: str = "xy"):
        """Initialize graphics."""
        self._model = model
        self._display_data = {}
        self._display_entity_data = {}
        self._plotter = None
        self._picker = None
        self._color_by_type = ColorByType.ZONE
        self._parts = None
        self._app = None
        self._ruler_visible = False
        self._ruler_actor = None
        self._colorByTypeBt: vtk.vtkButtonWidget = None
        self._hideBt: vtk.vtkButtonWidget = None
        self._showEdgeBt: vtk.vtkButtonWidget = None
        self._printInfoBt: vtk.vtkButtonWidget = None
        self._showRulerBt: vtk.vtkButtonWidget = None
        self._showInitVelBt: vtk.vtkButtonWidget = None
        self._viewIsoBt: vtk.vtkButtonWidget = None
        self._viewTopBt: vtk.vtkButtonWidget = None
        self._viewBottomBt: vtk.vtkButtonWidget = None
        self._viewFrontBt: vtk.vtkButtonWidget = None
        self._viewBackBt: vtk.vtkButtonWidget = None
        self._viewRightBt: vtk.vtkButtonWidget = None
        self._viewLeftBt: vtk.vtkButtonWidget = None
        self._sphinx_build = defaults.get_sphinx_build()
        self._use_trame = use_trame
        self._view_position = view_position
        self._init_velocity_data = []
        self._bdy_spc = []
        self._actor_init_velocity = None
        self._actor_bdy_spc = None

        if self._use_trame and not _HAS_TRAME:  # pragma: no cover
            warn_msg = (
                "'use_trame' is active but Trame dependencies are not installed."
                "Consider installing 'pyvista[trame]' to use this functionality."
            )
            # self._model._logger.warning(warn_msg)
        self.__update_display_data()

    def __update_display_data(self):
        """Update the objects displayed in the visualizer."""
        self._model._sync_up_model()
        part_ids = [part.id for part in self._model.parts]
        self._display_data.clear()
        for i, part_id in enumerate(part_ids):
            data = {}
            part = self._model.get_part(part_id)
            disp_mesh_data: list[_DisplayMesh] = [self.__get_face_display_mesh_object(part_id)]
            if len(disp_mesh_data) > 0:
                data["faces"] = disp_mesh_data
            self._display_data[part_id] = data
        self._init_velocity_data = self._model.get_init_velocity()
        self._bdy_spc = self._model.get_bdy_spc()
        # rigidwall
        self._display_entity_data.clear()
        num = len(self._model._rigidwall)
        for entity_id in range(1, num + 1):
            data = {}
            disp_mesh_data: list[_DisplayMesh] = [self.__get_entity_display_mesh_object(entity_id)]
            if len(disp_mesh_data) > 0:
                data["faces"] = disp_mesh_data
            self._display_entity_data[entity_id] = data

        # self._model._sync_up_model()

    def __get_face_display_mesh_object(self, part_id: int):
        """Display the faces in an object.

        Parameters
        ----------
        part_id : int
            ID of the part to show the edges on.

        Returns
        -------
        _DisplayMesh
            Displayed mesh.
        """
        part = self._model.get_part(part_id)
        node_coords = self._model.get_nodes()
        face_list = part.connectivity
        ptype = part.type
        if ptype == "SOLID" or ptype == "SHELL":
            meshtype = DisplayMeshType.FACE
        elif ptype == "BEAM":
            meshtype = DisplayMeshType.BEAM

        disp_mesh = _DisplayMesh(
            meshtype,
            part_id,
            self,
            self._model,
            node_coords,
            face_list,
            part_name=part.name,
        )
        return disp_mesh

    def __get_entity_display_mesh_object(self, entity_id: int):
        """Display the faces in an object.

        Parameters
        ----------
        entity_id : int
            ID of the entity to show the edges on.

        Returns
        -------
        _DisplayMesh
            Displayed mesh.
        """
        index = entity_id - 1
        rw = self._model._rigidwall[index]
        type = rw[0]
        if type == "rigidwall_cylinder":
            meshtype = DisplayMeshType.CYLINDER
            mesh = pv.Cylinder(
                center=[rw[1], rw[2], rw[3]], direction=[rw[4], rw[5], rw[6]], radius=rw[7], height=rw[8]
            )
        elif type == "rigidwall_sphere":
            meshtype = DisplayMeshType.SPHERE
            mesh = pv.Sphere(center=[rw[1], rw[2], rw[3]], direction=[rw[4], rw[5], rw[6]], radius=rw[7])
        elif type == "rigidwall_planar":
            meshtype = DisplayMeshType.PLANAR
            dir = [rw[4] - rw[1], rw[5] - rw[2], rw[6] - rw[3]]
            mesh = pv.Plane(center=[rw[1], rw[2], rw[3]], direction=dir, i_size=1000, j_size=1000)

        disp_mesh = _DisplayMesh(
            type=meshtype,
            part_id=entity_id,
            graphics=self,
            model=self._model,
            mesh=mesh,
        )
        return disp_mesh

    def __call__(
        self,
        parts: List = None,
        update: bool = True,
    ):
        """Show the appropriate display based on parameters.

        Parameters
        ----------
        parts : Any, optional
            Parts to show. The default is ``None``.
        update : bool, optional
            Whether to update the display. The default is ``True``.
        """
        parts = [part.id for part in self._model.parts]
        self.__draw_parts(parts, False)
        # self.show()

    def __color_by_type_callback(self, button_value: bool):  # pragma: no cover
        """Determine the color of a type in the callback."""
        vr = self._colorByTypeBt.GetRepresentation()
        state = vr.GetState()
        self._color_by_type = ColorByType(state)
        r = vtk.vtkPNGReader()
        color_by_type_icon_file = ""
        if self._color_by_type == ColorByType.ZONELET:
            color_by_type_icon_file = os.path.join(os.path.dirname(__file__), "images", "surface_body.png")
        elif self._color_by_type == ColorByType.ZONE:
            color_by_type_icon_file = os.path.join(os.path.dirname(__file__), "images", "bin.png")
        else:
            color_by_type_icon_file = os.path.join(os.path.dirname(__file__), "images", "parts.png")
        r.SetFileName(color_by_type_icon_file)
        r.Update()
        image = r.GetOutput()
        vr.SetButtonTexture(state, image)
        [
            disp_mesh.set_color_by_type(self._color_by_type)
            for part_id, data in self._display_data.items()
            if data.get("faces") != None
            for disp_mesh in data["faces"]
        ]

    def __show_init_velocity_callback(self, button_value: bool):  # pragma: no cover
        """Show initial velocity in the callback."""
        if len(self._init_velocity_data) == 0:
            return
        vr = self._showInitVelBt.GetRepresentation()
        state = vr.GetState()
        r = vtk.vtkPNGReader()
        color_by_type_icon_file = ""
        if button_value:
            color_by_type_icon_file = os.path.join(os.path.dirname(__file__), "images", "surface_body.png")
            list1 = self._init_velocity_data[: len(self._init_velocity_data) // 2]
            list2 = self._init_velocity_data[len(self._init_velocity_data) // 2 :]
            vertices = np.array(list1)
            vel = np.array(list2)
            self._actor_init_velocity = self._plotter.add_arrows(vertices, vel)
        else:
            color_by_type_icon_file = os.path.join(os.path.dirname(__file__), "images", "bin.png")
            self._plotter.remove_actor(self._actor_init_velocity)
            self._actor_init_velocity = None
        r.SetFileName(color_by_type_icon_file)
        r.Update()
        image = r.GetOutput()
        vr.SetButtonTexture(state, image)

    def __show_bdy_spc_callback(self, button_value: bool):  # pragma: no cover
        """Show boundary spc in the callback."""
        if len(self._bdy_spc) == 0:
            return
        vr = self._colorByTypeBt.GetRepresentation()
        state = vr.GetState()
        r = vtk.vtkPNGReader()
        color_by_type_icon_file = ""
        if button_value:
            color_by_type_icon_file = os.path.join(os.path.dirname(__file__), "images", "surface_body.png")
            vertices = np.array(self._bdy_spc)
            self._actor_bdy_spc = self._plotter.add_points(vertices, color="red")
        else:
            color_by_type_icon_file = os.path.join(os.path.dirname(__file__), "images", "bin.png")
            self._plotter.remove_actor(self._actor_bdy_spc)
            self._actor = None
        r.SetFileName(color_by_type_icon_file)
        r.Update()
        image = r.GetOutput()
        vr.SetButtonTexture(state, image)

    def __hide_unhide_selection(self, button_value: bool):  # pragma: no cover
        """Hide or unhide the clicked component."""
        sel_disp_mesh = self._picker.selections
        if len(sel_disp_mesh) > 0:
            [disp_mesh.hide(self._plotter) for disp_mesh in sel_disp_mesh]
            self._picker.clear_selection()
        else:
            [
                disp_mesh.unhide(self._plotter)
                for part_id, data in self._display_data.items()
                if data.get("faces") != None
                for disp_mesh in data["faces"]
            ]

    def __show_edges_callback(self, flag):  # pragma: no cover
        """Show or hide the edges."""
        [
            disp_mesh.show_edges(flag)
            for part_id, data in self._display_data.items()
            if data.get("faces") != None
            for disp_mesh in data["faces"]
        ]

    def __print_callback(self, flag):  # pragma: no cover
        """Print information."""
        sel_disp_mesh = self._picker.selections
        [print(disp_mesh) for disp_mesh in sel_disp_mesh]

    def __show_ruler_callback(self, flag):  # pragma: no cover
        """Show a ruler on the UI when ruler button is clicked."""
        if self._plotter is not None:
            if self._ruler_visible and self._ruler_actor is not None:
                self._plotter.remove_actor(self._ruler_actor)
                self._ruler_visible = False
            else:
                self._ruler_actor = self._plotter.show_bounds(
                    grid="front",
                    location="outer",
                    all_edges=False,
                    show_xaxis=True,
                    show_yaxis=True,
                    show_zaxis=True,
                )
                self._ruler_visible = True

    def __view_iso_callback(self, flag):  # pragma: no cover
        """View iso."""
        self._plotter.view_isometric()

    def __view_top_callback(self, flag):  # pragma: no cover
        """View top."""
        self._plotter.view_xy()

    def __view_bottom_callback(self, flag):  # pragma: no cover
        """View bottom."""
        self._plotter.view_yx()

    def __view_front_callback(self, flag):  # pragma: no cover
        """View front."""
        self._plotter.view_yz()

    def __view_back_callback(self, flag):  # pragma: no cover
        """View back."""
        self._plotter.view_zy()

    def __view_right_callback(self, flag):  # pragma: no cover
        """View right."""
        self._plotter.view_xz()

    def __view_left_callback(self, flag):  # pragma: no cover
        """View left."""
        self._plotter.view_zx()

    def get_face_mesh_data(self):
        """Get the mesh data from a face.

        Returns
        -------
        List
            Mesh data for the face.
        """
        face_mesh_data = [
            disp_mesh
            for part_id, data in self._display_data.items()
            if data.get("faces") != None
            for disp_mesh in data["faces"]
        ]
        return face_mesh_data

    def show(self):
        """Show the current set display."""
        solidlist = self._model.get_solid_elements()
        shelllist = self._model.get_shell_elements()
        nodes = self._model.get_nodes()
        vertices = np.array(nodes)
        faces = []
        for elem in solidlist:
            face1 = [4, elem[0] - 1, elem[1] - 1, elem[2] - 1, elem[3] - 1]
            face2 = [4, elem[0] - 1, elem[4] - 1, elem[5] - 1, elem[1] - 1]
            face3 = [4, elem[1] - 1, elem[5] - 1, elem[6] - 1, elem[2] - 1]
            face4 = [4, elem[2] - 1, elem[6] - 1, elem[7] - 1, elem[3] - 1]
            face5 = [4, elem[3] - 1, elem[7] - 1, elem[4] - 1, elem[0] - 1]
            face6 = [4, elem[4] - 1, elem[7] - 1, elem[6] - 1, elem[5] - 1]
            faces.append(face1)
            faces.append(face2)
            faces.append(face3)
            faces.append(face4)
            faces.append(face5)
            faces.append(face6)
        for elem in shelllist:
            face = [4, elem[0] - 1, elem[1] - 1, elem[2] - 1, elem[3] - 1]
            faces.append(face)
        fs = np.hstack(faces)
        surf = pv.PolyData(vertices, fs)
        self._plotter = pv.Plotter()
        self._plotter.add_mesh(surf, show_edges=True, line_width=5)
        self._plotter.show_axes()
        if self._sphinx_build == False:
            self._colorByTypeBt = self._plotter.add_checkbox_button_widget(
                self.__color_by_type_callback,
                position=(10, 700),
                size=30,
                border_size=3,
            )
            self._showInitVelBt = self._plotter.add_checkbox_button_widget(
                self.__show_init_velocity_callback,
                position=(10, 700),
                size=30,
                border_size=3,
            )
            self._hideBt = self._plotter.add_checkbox_button_widget(
                self.__hide_unhide_selection, position=(10, 650), size=30, border_size=3
            )
            self._showEdgeBt = self._plotter.add_checkbox_button_widget(
                self.__show_edges_callback, value=False, position=(10, 600), size=30, border_size=3
            )
            self._printInfoBt = self._plotter.add_checkbox_button_widget(
                self.__print_callback, position=(10, 550), size=30, border_size=3
            )
            self._showRulerBt = self._plotter.add_checkbox_button_widget(
                self.__show_ruler_callback, position=(10, 500), size=30, border_size=3
            )
        self._plotter.camera_position = "xy"
        self._picker = Picker(self._plotter, self)
        self._plotter.track_click_position(self._picker, side="left")
        if self._sphinx_build == False:
            self.__update_bt_icons()
        self._plotter.show()

    def __draw_parts(self, parts: List = [], update: bool = False, spline: bool = False):
        """Draw parts in the display.

        Parameters
        ----------
        parts : list, optional
            List of parts to display. The default is ``[]``.
        update : bool, optional
             Whether to update the display. The default is ``False``.
        spline : bool, optional
            Whether to use splines. The default is ``False``.
        """
        if update == True:
            self.__update_display_data()
        self._plotter = pv.Plotter()
        self._plotter.show_axes()
        # disp_mesh.add_to_plotter(self._plotter)
        [
            disp_mesh.add_to_plotter(self._plotter)
            for part_id, data in self._display_data.items()
            if (part_id in parts)
            for key, disp_mesh_data in data.items()
            for disp_mesh in disp_mesh_data
        ]

        [
            disp_mesh.add_to_plotter(self._plotter)
            for entity_id, data in self._display_entity_data.items()
            for key, disp_mesh_data in data.items()
            for disp_mesh in disp_mesh_data
        ]
        if self._sphinx_build == False:
            self._colorByTypeBt = self._plotter.add_checkbox_button_widget(
                self.__show_bdy_spc_callback,
                position=(10, 700),
                size=30,
                border_size=3,
            )
            self._showInitVelBt = self._plotter.add_checkbox_button_widget(
                self.__show_init_velocity_callback,
                position=(10, 650),
                size=30,
                border_size=3,
            )
            self._hideBt = self._plotter.add_checkbox_button_widget(
                self.__hide_unhide_selection, position=(10, 600), size=30, border_size=3
            )
            self._showEdgeBt = self._plotter.add_checkbox_button_widget(
                self.__show_edges_callback, value=False, position=(10, 550), size=30, border_size=3
            )
            self._printInfoBt = self._plotter.add_checkbox_button_widget(
                self.__print_callback, position=(10, 500), size=30, border_size=3
            )
            self._showRulerBt = self._plotter.add_checkbox_button_widget(
                self.__show_ruler_callback, position=(10, 450), size=30, border_size=3
            )
            self._viewIsoBt = self._plotter.add_checkbox_button_widget(
                self.__view_iso_callback, position=(700, 10), size=30, border_size=3
            )
            self._viewTopBt = self._plotter.add_checkbox_button_widget(
                self.__view_top_callback, position=(650, 10), size=30, border_size=3
            )
            self._viewBottomBt = self._plotter.add_checkbox_button_widget(
                self.__view_bottom_callback, position=(600, 10), size=30, border_size=3
            )
            self._viewFrontBt = self._plotter.add_checkbox_button_widget(
                self.__view_front_callback, position=(550, 10), size=30, border_size=3
            )
            self._viewBackBt = self._plotter.add_checkbox_button_widget(
                self.__view_back_callback, position=(500, 10), size=30, border_size=3
            )
            self._viewRightBt = self._plotter.add_checkbox_button_widget(
                self.__view_right_callback, position=(450, 10), size=30, border_size=3
            )
            self._viewLeftBt = self._plotter.add_checkbox_button_widget(
                self.__view_left_callback, position=(400, 10), size=30, border_size=3
            )
        self._picker = Picker(self._plotter, self)
        self._plotter.track_click_position(self._picker, side="left")
        if self._sphinx_build == False:
            self.__update_bt_icons()
        self._show_selector()

    def _show_selector(self):
        """Chooses between using Trame or Python visualizer."""
        if self._use_trame:  # pragma: no cover
            pv.set_jupyter_backend("server")
            visualizer = TrameVisualizer()
            visualizer.set_scene(self._plotter)
            visualizer.show()
        else:
            if self._view_position in ["xy", "xz", "yx", "yz", "zx", "zy"]:
                pos = self._view_position
            else:
                pos = "xy"
            self._plotter.camera_position = pos
            self._plotter.show(jupyter_backend="static")

    def __update_bt_icons(self):
        """Update the icons on display."""
        vr = self._colorByTypeBt.GetRepresentation()
        vr.SetNumberOfStates(2)
        r = vtk.vtkPNGReader()
        color_by_zone_icon_file = os.path.join(os.path.dirname(__file__), "images", "bin.png")
        r.SetFileName(color_by_zone_icon_file)
        r.Update()
        image_1 = r.GetOutput()
        vr.SetButtonTexture(0, image_1)

        vr = self._showInitVelBt.GetRepresentation()
        vr.SetNumberOfStates(2)
        r = vtk.vtkPNGReader()
        color_by_zone_icon_file = os.path.join(os.path.dirname(__file__), "images", "bin.png")
        r.SetFileName(color_by_zone_icon_file)
        r.Update()
        image_1 = r.GetOutput()
        vr.SetButtonTexture(0, image_1)

        hide_vr = self._hideBt.GetRepresentation()
        hide_unhide_icon_file = os.path.join(os.path.dirname(__file__), "images", "invert_visibility.png")
        hide_r = vtk.vtkPNGReader()
        hide_r.SetFileName(hide_unhide_icon_file)
        hide_r.Update()
        image_2 = hide_r.GetOutput()
        hide_vr.SetButtonTexture(0, image_2)
        hide_vr.SetButtonTexture(1, image_2)
        show_edge_vr = self._showEdgeBt.GetRepresentation()
        show_edges_icon_file = os.path.join(os.path.dirname(__file__), "images", "show_edges.png")
        show_edge_r = vtk.vtkPNGReader()
        show_edge_r.SetFileName(show_edges_icon_file)
        show_edge_r.Update()
        image_3 = show_edge_r.GetOutput()
        show_edge_vr.SetButtonTexture(0, image_3)
        show_edge_vr.SetButtonTexture(1, image_3)

        print_info_vr = self._printInfoBt.GetRepresentation()
        print_info_icon_file = os.path.join(os.path.dirname(__file__), "images", "selectioninfo.png")
        print_info_r = vtk.vtkPNGReader()
        print_info_r.SetFileName(print_info_icon_file)
        print_info_r.Update()
        image_4 = print_info_r.GetOutput()
        print_info_vr.SetButtonTexture(0, image_4)
        print_info_vr.SetButtonTexture(1, image_4)

        show_ruler_vr = self._showRulerBt.GetRepresentation()
        show_ruler_icon_file = os.path.join(os.path.dirname(__file__), "images", "show_ruler.png")
        show_ruler_r = vtk.vtkPNGReader()
        show_ruler_r.SetFileName(show_ruler_icon_file)
        show_ruler_r.Update()
        image_5 = show_ruler_r.GetOutput()
        show_ruler_vr.SetButtonTexture(0, image_5)
        show_ruler_vr.SetButtonTexture(1, image_5)

        show_viewiso_vr = self._viewIsoBt.GetRepresentation()
        viewiso_icon_file = os.path.join(os.path.dirname(__file__), "images", "iso_metric.png")
        show_view_r = vtk.vtkPNGReader()
        show_view_r.SetFileName(viewiso_icon_file)
        show_view_r.Update()
        image_6 = show_view_r.GetOutput()
        show_viewiso_vr.SetButtonTexture(0, image_6)
        show_viewiso_vr.SetButtonTexture(1, image_6)

        show_viewtop_vr = self._viewTopBt.GetRepresentation()
        viewtop_icon_file = os.path.join(os.path.dirname(__file__), "images", "iso_top.png")
        show_viewtop_r = vtk.vtkPNGReader()
        show_viewtop_r.SetFileName(viewtop_icon_file)
        show_viewtop_r.Update()
        image_7 = show_viewtop_r.GetOutput()
        show_viewtop_vr.SetButtonTexture(0, image_7)
        show_viewtop_vr.SetButtonTexture(1, image_7)

        show_viewbottom_vr = self._viewBottomBt.GetRepresentation()
        viewbottom_icon_file = os.path.join(os.path.dirname(__file__), "images", "iso_bottom.png")
        show_viewbottom_r = vtk.vtkPNGReader()
        show_viewbottom_r.SetFileName(viewbottom_icon_file)
        show_viewbottom_r.Update()
        image_8 = show_viewbottom_r.GetOutput()
        show_viewbottom_vr.SetButtonTexture(0, image_8)
        show_viewbottom_vr.SetButtonTexture(1, image_8)

        show_viewfront_vr = self._viewFrontBt.GetRepresentation()
        viewfront_icon_file = os.path.join(os.path.dirname(__file__), "images", "iso_front.png")
        show_viewfront_r = vtk.vtkPNGReader()
        show_viewfront_r.SetFileName(viewfront_icon_file)
        show_viewfront_r.Update()
        image_9 = show_viewfront_r.GetOutput()
        show_viewfront_vr.SetButtonTexture(0, image_9)
        show_viewfront_vr.SetButtonTexture(1, image_9)

        show_viewback_vr = self._viewBackBt.GetRepresentation()
        viewback_icon_file = os.path.join(os.path.dirname(__file__), "images", "iso_back.png")
        show_viewback_r = vtk.vtkPNGReader()
        show_viewback_r.SetFileName(viewback_icon_file)
        show_viewback_r.Update()
        image_10 = show_viewback_r.GetOutput()
        show_viewback_vr.SetButtonTexture(0, image_10)
        show_viewback_vr.SetButtonTexture(1, image_10)

        show_viewright_vr = self._viewRightBt.GetRepresentation()
        viewright_icon_file = os.path.join(os.path.dirname(__file__), "images", "iso_right.png")
        show_viewright_r = vtk.vtkPNGReader()
        show_viewright_r.SetFileName(viewright_icon_file)
        show_viewright_r.Update()
        image_11 = show_viewright_r.GetOutput()
        show_viewright_vr.SetButtonTexture(0, image_11)
        show_viewright_vr.SetButtonTexture(1, image_11)

        show_viewleft_vr = self._viewLeftBt.GetRepresentation()
        viewleft_icon_file = os.path.join(os.path.dirname(__file__), "images", "iso_left.png")
        show_viewleft_r = vtk.vtkPNGReader()
        show_viewleft_r.SetFileName(viewleft_icon_file)
        show_viewleft_r.Update()
        image_12 = show_viewleft_r.GetOutput()
        show_viewleft_vr.SetButtonTexture(0, image_12)
        show_viewleft_vr.SetButtonTexture(1, image_12)

    def get_color_by_type(self) -> ColorByType:
        """Get the color by zone type.

        Returns
        -------
        ColorByType
            Color by zone type.
        """
        return self._color_by_type


class _DisplayMesh(object):  # pragma: no cover
    """Provides a helper class for displaying meshes in the plotter.

    Parameters
    ----------
    type : DisplayMeshType
        Type of the mesh.
    id : int
        ID of the mesh.
    part_id : int
        ID of the part to mesh.
    graphics : Graphics
        Instance of the ``Graphics`` class.
    model : pre.Model
        Model to show.
    vertices : np.array
        Vertices of the mesh.
    facet_list : np.array
        List of faces of the model.
    part_name : str, optional
        Name of the part. The default is ``""``.
    """

    def __init__(
        self,
        type: DisplayMeshType,
        part_id: int,
        graphics: Graphics,
        model: pre.Model,
        vertices: np.array = None,
        facet_list: np.array = None,
        mesh=None,
        part_name: str = "",
    ):
        """Initialize the parameters to display."""
        self._type = type
        self._part_id = part_id
        self._graphics = graphics
        self._model = model
        self._vertices = vertices
        self._facet_list = facet_list
        self._mesh = mesh
        self._poly_data = None
        self._actor = None
        self._part_name = part_name
        # self.__update()

    @property
    def poly_data(self):
        return self._poly_data

    def __str__(self):
        msg = "Part Id : " + str(self._part_id) + ", Part Name : " + self._part_name + "\n"
        return msg

    def add_to_plotter(self, plotter: Plotter):
        """Add elements to the plotter.

        Parameters
        ----------
        plotter : Plotter
            Elements of another plotter to add.
        """

        if self._poly_data == None:
            if self._type is DisplayMeshType.FACE:
                surf = pv.PolyData(self._vertices, self._facet_list)
                fcolor = np.array(self.get_face_color())
                colors = np.tile(fcolor, (surf.n_cells, 1))
                surf["colors"] = colors
                surf.disp_mesh = self
                self._poly_data = surf
                # sh_edge = self._has_mesh if self._type is DisplayMeshType.TOPOFACE else True
                # if self._poly_data.n_points > 0:
                self._actor = plotter.add_mesh(
                    self._poly_data, show_edges=True, scalars="colors", rgb=True, pickable=True
                )
            elif self._type is DisplayMeshType.BEAM:
                surf = pv.PolyData(self._vertices, lines=self._facet_list)
                fcolor = np.array(self.get_face_color())
                colors = np.tile(fcolor, (surf.n_cells, 1))
                surf["colors"] = colors
                surf.disp_mesh = self
                self._poly_data = surf
                self._actor = plotter.add_mesh(
                    self._poly_data, show_edges=True, scalars="colors", rgb=True, pickable=True
                )
            elif (
                self._type is DisplayMeshType.CYLINDER
                or self._type is DisplayMeshType.SPHERE
                or self._type is DisplayMeshType.PLANAR
            ):
                surf = self._mesh
                fcolor = np.array(self.get_face_color())
                colors = np.tile(fcolor, (surf.n_cells, 1))
                surf["colors"] = colors
                surf.disp_mesh = self
                self._poly_data = surf
                self._actor = plotter.add_mesh(
                    self._poly_data, show_edges=True, scalars="colors", rgb=True, pickable=True
                )
                return

    def get_face_color(self):
        """Get the colors of faces.

        Returns
        -------
        List
            List of colors for faces.
        """
        type = self._graphics.get_color_by_type()
        num_colors = int(color_matrix.size / 3)
        return color_matrix[self._part_id % num_colors].tolist()

    def get_edge_color(self):
        """Get the colors of edges.

        Returns
        -------
        List
            List of colors for edges.
        """
        num_colors = int(color_matrix.size / 3)
        if self._type == DisplayMeshType.FACE:
            return color_matrix[self._id % num_colors].tolist()
        elif self._type == DisplayMeshType.BEAM:
            if self._topo_edge_type == 1:
                return [255, 0, 0]
            elif self._topo_edge_type == 2:
                return [0, 0, 0]
            elif self._topo_edge_type == 3:
                return [0, 255, 255]
            elif self._topo_edge_type == 4:
                return [255, 0, 255]
            elif self._topo_edge_type == 5:
                return [255, 255, 0]
            elif self._topo_edge_type == 6:
                return [128, 0, 128]
            else:
                return color_matrix[self._id % num_colors].tolist()

    def deselect(self):
        """Deselect mesh."""
        if self._type == DisplayMeshType.FACE:
            if self._poly_data != None and self._actor != None:
                self._actor.prop.edge_color = "black"
                # fcolor = np.array(self.get_face_color())
                # colors = np.tile(fcolor, (self._poly_data.n_faces, 1))
                # self._poly_data["colors"] = colors

    def select(self):
        """Select mesh."""
        if self._type == DisplayMeshType.FACE:
            if self._poly_data != None and self._actor != None:
                self._actor.prop.edge_color = "white"
                # fcolor = np.array([255, 255, 0])
                # fcolor = np.array([233, 99, 28])
                # colors = np.tile(fcolor, (self._poly_data.n_faces, 1))
                # self._poly_data["colors"] = colors

    def hide(self, plotter: Plotter):
        """Hide mesh."""
        plotter.remove_actor(self._actor)
        self._actor = None

    def clear(self, plotter: Plotter):
        """Clear mesh."""
        plotter.remove_actor(self._actor)
        del self._actor
        self._actor = None

    def unhide(self, plotter: Plotter):
        """Unhide mesh."""
        if self._actor is None and self._poly_data.n_points > 0:
            self._actor = plotter.add_mesh(
                self._poly_data,
                show_edges=True,
                scalars="colors",
                rgb=True,
                pickable=True,
            )

    def show_edges(self, show: bool):
        """Show edges."""
        if self._actor is not None:
            prop = self._actor.GetProperty()
            self._show_edges = not prop.GetEdgeVisibility()
            prop.SetEdgeVisibility(not prop.GetEdgeVisibility())

    def set_color_by_type(self, type: ColorByType):
        """Set the color based on the zone type.

        Parameters
        ----------
        type : ColorByType
            Color by zone type.
        """
        if self._type == DisplayMeshType.FACE:
            if self._poly_data != None:
                fcolor = np.array(self.get_face_color())
                colors = np.tile(fcolor, (self._poly_data.n_cells, 1))
                self._poly_data["colors"] = colors
