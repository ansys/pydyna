"""Module containing the ``Part`` class."""
from typing import List


class Part(object):
    """
    Defines and modifies the parts of a model.

    Parameters
    ----------
    model: ansys.dyna.core.pre.Model
        Model in which the part is created.
    id: int
        ID of the part provided by the server.
    name: str
        Part name.
    """

    def __init__(self, model, id: int, name: str, type: str, conn: List):
        """Initialize Part."""
        self._model = model
        self._print_mesh = False
        self._print_id = False
        self._id = id
        self._name = name
        self._type = type
        self._connectivity: List = conn

    @property
    def id(self) -> int:
        return self._id

    @property
    def name(self) -> str:
        return self._name

    @property
    def type(self) -> str:
        return self._type

    @property
    def connectivity(self) -> List:
        return self._connectivity

    @property
    def id(self):
        """Get the id of Part."""
        return self._id

    @property
    def name(self):
        """Get the name of Part."""
        return self._name

    @property
    def print_mesh(self) -> bool:
        """Whether the mesh summary is set to print along with the part summary."""
        return self._print_mesh

    @print_mesh.setter
    def print_mesh(self, value: bool):
        """Print the mesh of the part.

        Parameters
        ----------
        value : bool
            Whether to print the mesh.
        """
        self._print_mesh = value

    @property
    def print_id(self) -> bool:
        """Whether IDs of TopoEntities or zonelets are set to print along with the part summary."""
        return self._print_id

    @print_id.setter
    def print_id(self, value: bool):
        self._print_id = value
