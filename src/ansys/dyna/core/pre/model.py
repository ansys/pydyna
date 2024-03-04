"""Module containing the managing logic of the PyDYNA model."""

from typing import List

from ansys.api.dyna.v0.kwprocess_pb2 import *  # noqa : F403
from ansys.api.dyna.v0.kwprocess_pb2_grpc import *  # noqa : F403

# from .kwprocess_pb2 import *
# from .kwprocess_pb2_grpc import *
from ansys.dyna.core.pre.part import Part


class Model:
    """Contains all information about Ansys PyDYNA Model."""

    def __init__(self, stub):
        """Initialize the model and the parameters."""
        self.stub = stub
        self._parts = []
        self._solids = []
        self._shells = []
        self._nodes = []
        self._bdy_spc: List = []
        self._init_velocity: List = []
        self._rigidwall = []
        # self._freeze()

    def add_bdy_spc(self, nodes):
        """Add boundary spc nodes."""
        self._bdy_spc = nodes

    def add_init_velocity(self, nodes):
        """Add initial velocity nodes."""
        self._init_velocity.append(nodes)

    def add_rigidwall(self, data: List):
        """Add rigidwall data."""
        self._rigidwall.append(data)

    def get_solid_elements(self) -> List:
        """Get the solid elements.

        Returns
        -------
        List
            solid element connectivity,list = [[n1,n2,n3,n4,n5,n6,n7,n8],[...],...]

        """
        cons = self.stub.GetSolidElements(GetSolidElementsRequest())
        num = 8
        lscons = cons.nodeids
        solidlist = [lscons[i : i + num] for i in range(0, len(lscons), num)]
        return solidlist

    def get_shell_elements(self) -> List:
        """Get the shell elements.

        Returns
        -------
        list
            list[0],solid element connectivity,list[0] = [[n1,n2,n3,n4,n5,n6,n7,n8],[...],...]

        """
        shells = self.stub.GetShellElements(GetShellElementsRequest())
        num = 4
        lscons = shells.nodeids
        shelllist = [lscons[i : i + num] for i in range(0, len(lscons), num)]
        return shelllist

    def get_nodes(self) -> List:
        """Get nodes.

        Returns
        -------
        list
            node coordinates,list = [[x1,y1,z1],[x2,y2,z2],...]

        """
        nodes = self.stub.GetNodes(GetNodesRequest())
        numconn = 3
        lsnodes = nodes.coords
        nlist = [lsnodes[i : i + numconn] for i in range(0, len(lsnodes), numconn)]
        return nlist

    def get_part(self, id: int) -> Part:
        """Get the part by ID.

        Parameters
        ----------
        id : int
            ID of the part.

        Returns
        -------
        Part
            Part or ``None`` if the given part ID doesn't exist.

        """
        for part in self._parts:
            if part.id == id:
                return part
        return None

    def get_init_velocity(self) -> List:
        """Get initial velocity data."""
        nids = [i[0] for i in self._init_velocity]
        data = self.stub.GetNodesCoord(GetNodesCoordRequest(nodeids=nids))
        num = 3
        coord = data.coords
        nlist1 = [coord[i : i + num] for i in range(0, len(coord), num)]
        nlist2 = [[vel[1], vel[2], vel[3]] for vel in self._init_velocity]
        return nlist1 + nlist2

    def get_bdy_spc(self) -> List:
        """Get boundary spc data."""
        nids = self._bdy_spc
        data = self.stub.GetNodesCoord(GetNodesCoordRequest(nodeids=nids))
        num = 3
        coord = data.coords
        nlist = [coord[i : i + num] for i in range(0, len(coord), num)]
        return nlist

    def get_rigidwall(self, id):
        """Get rigidwall data."""

        data = self._rigidwall(id - 1)
        return data
        data = self.stub.GetRigidWall(GetRigidWallRequest(id=id))
        geomtype = data.geomtype
        param = data.parameter
        rigidwall = [geomtype]
        if geomtype == 3:
            num = 8
        elif geomtype == 4:
            num = 7
        for i in range(num):
            rigidwall.append(param[i])
        return rigidwall

    def _sync_up_model(self):
        """Synchronize the client model with the server model.

        This method Updates proxy child objects of the client model with the
        child objects of the server model.

        """
        shells = self.stub.GetPart(GetPartRequest(type="SHELL"))
        num = 6
        data = shells.data
        shell_dict = {}
        for i in range(0, len(data), num):
            mat = data[i + 1]
            face = [4, data[i + 2] - 1, data[i + 3] - 1, data[i + 4] - 1, data[i + 5] - 1]
            if mat in shell_dict.keys():
                shell_dict[mat][1].append(face)
            else:
                info = self.stub.GetPartInfo(GetPartInfoRequest(ids=[mat]))
                name = info.names[0]
                extid = info.extids[0]
                shell_dict[mat] = [[name, extid], [face]]
        for pid, conn in shell_dict.items():
            self._parts.append(Part(self, conn[0][1], conn[0][0], "SHELL", conn[1]))

        solids = self.stub.GetPart(GetPartRequest(type="SOLID"))
        num = 10
        data = solids.data
        solid_dict = {}
        for i in range(0, len(data), num):
            mat = data[i + 1]
            elem0 = data[i + 2] - 1
            elem1 = data[i + 3] - 1
            elem2 = data[i + 4] - 1
            elem3 = data[i + 5] - 1
            elem4 = data[i + 6] - 1
            elem5 = data[i + 7] - 1
            elem6 = data[i + 8] - 1
            elem7 = data[i + 9] - 1
            face1 = [4, elem0, elem1, elem2, elem3]
            face2 = [4, elem0, elem4, elem5, elem1]
            face3 = [4, elem1, elem5, elem6, elem2]
            face4 = [4, elem2, elem6, elem7, elem3]
            face5 = [4, elem3, elem7, elem4, elem0]
            face6 = [4, elem4, elem7, elem6, elem5]
            if mat in solid_dict.keys():
                solid_dict[mat][1].append(face1)
                solid_dict[mat][1].append(face2)
                solid_dict[mat][1].append(face3)
                solid_dict[mat][1].append(face4)
                solid_dict[mat][1].append(face5)
                solid_dict[mat][1].append(face6)
            else:
                info = self.stub.GetPartInfo(GetPartInfoRequest(ids=[mat]))
                name = info.names[0]
                extid = info.extids[0]
                solid_dict[mat] = [[name, extid], [face1, face2, face3, face4, face5, face6]]
        for pid, conn in solid_dict.items():
            self._parts.append(Part(self, conn[0][1], conn[0][0], "SOLID", conn[1]))

        beams = self.stub.GetPart(GetPartRequest(type="BEAM"))
        num = 5
        data = beams.data
        beam_dict = {}
        for i in range(0, len(data), num):
            mat = data[i + 1]
            line = [2, data[i + 2] - 1, data[i + 3] - 1]
            if mat in beam_dict.keys():
                beam_dict[mat][1].append(line)
            else:
                info = self.stub.GetPartInfo(GetPartInfoRequest(ids=[mat]))
                name = info.names[0]
                extid = info.extids[0]
                beam_dict[mat] = [[name, extid], [line]]
        for pid, conn in beam_dict.items():
            self._parts.append(Part(self, conn[0][1], conn[0][0], "BEAM", conn[1]))
        return
        rigid_num = self.stub.GetNum(GetNumRequest(type="rigidwall"))
        num = rigid_num.num
        for i in range(1, num + 1):
            rigid = self.get_rigidwall(id=i)
            self._rigidwall.append(rigid)
        return

    @property
    def parts(self) -> List[Part]:
        """Get the list of parts for the model.

        Returns
        -------
        List[Part]
            List of parts for the model.

        """
        return self._parts
