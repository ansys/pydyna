"""
Belted dummy example
=====================

This example show how to create an Belted dummy model with Pydyna-pre module
"""

import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../ansys/dyna'))
from pre.dynabase import DynaBase

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    dummy = DynaBase(hostname=hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "belted_dummy" + os.sep
    fns.append(path + "belted_dummy.k")
    dummy.open_files(fns)

    dummy.create_termination(endtim=0.12)
    dummy.create_control_contact(rwpnal=0, orien=2)
    dummy.create_timestep(tssfac=0.8)
    dummy.create_database_binary(dt=2.5e-3)
    vel = [14.8, 0, 0, 0, 0, 0]
    dummy.create_init_vel(nsid=0, velocity=vel)

    abs = [
        0.00000000e00,
        4.00000019e-03,
        6.00000005e-03,
        9.99999978e-03,
        1.35000004e-02,
        1.79999992e-02,
        2.34999992e-02,
        3.09999995e-02,
        3.40000018e-02,
        3.59999985e-02,
        3.99999991e-02,
        4.19999994e-02,
        4.45000008e-02,
        4.69999984e-02,
        4.89999987e-02,
        5.15000001e-02,
        5.40000014e-02,
        7.99999982e-02,
        9.00000036e-02,
        9.49999988e-02,
        9.74999964e-02,
        1.03000000e-01,
        1.08999997e-01,
        1.15000002e-01,
        1.19499996e-01,
        1.12499997e-01,
        1.29999995e-01,
        1.41000003e-01,
        1.49000004e-01,
        1.51999995e-01,
    ]
    ord = [
        0.00000000e00,
        4.41500015e01,
        2.94300003e01,
        7.84800034e01,
        4.41500015e01,
        1.37300003e02,
        1.66800003e02,
        2.35399994e02,
        2.35399994e02,
        2.55100006e02,
        2.60000000e02,
        2.60000000e02,
        2.69799988e02,
        2.60000000e02,
        2.60000000e02,
        2.55100006e02,
        2.55100006e02,
        1.22599998e02,
        4.90499992e01,
        -9.81000042e00,
        -2.45300007e01,
        1.47200003e01,
        -1.47200003e01,
        -9.81000042e00,
        -4.90999985e00,
        -9.81000042e00,
        -2.45000005e00,
        -1.96200008e01,
        -2.45000005e00,
        -1.47200003e01,
    ]
    dummy.create_definecurve(lcid=50, sfo=1, abscissa=abs, ordinate=ord)

    nodeslist = [
        1763,
        1764,
        1765,
        1766,
        1767,
        1768,
        1769,
        1770,
        1771,
        1772,
        1773,
        1774,
        1775,
        1776,
        1777,
        1778,
        1779,
        1780,
        1781,
        1782,
        1783,
        1784,
        1785,
        1786,
        1787,
        1788,
        1789,
        1792,
        1795,
        1904,
        1906,
        1908,
        1909,
        1910,
        1997,
        1998,
        1999,
        2043,
    ]
    dummy.create_nodeset(option="LIST", sid=1, entities=nodeslist)
    dummy.create_boundary_prescribed_motion(
        id=1,
        heading="motion",
        option="SET",
        typeid=1,
        dof=1,
        vad=1,
        lcid=50,
        sf=-1,
        vid=1,
    )

    abs = [0, 0.152]
    ord = [9.81, 9.81]
    dummy.create_definecurve(lcid=51, sfo=1, abscissa=abs, ordinate=ord)
    dummy.create_load_body(option="Z", lcid=51)

    cid = 1
    fslist = [0.62, 0.62, 0.62, 0.8, 1, 0.8, 0.88, 0.88, 0.16, 0.88, 0]
    for i in range(1, 12):
        title = "contact" + str(i)
        dummy.create_contact(
            cid=i,
            title=title,
            option1="SURFACE_TO_SURFACE",
            option3=False,
            ssid=cid,
            msid=cid + 1,
            sstyp=0,
            mstyp=0,
            fs=fslist[i - 1],
        )
        cid += 2

    nodeslist = [
        [99, 227],
        [228, 405],
        [406, 865],
        [866, 971],
        [407, 537],
        [538, 685],
        [408, 603],
        [604, 763],
        [972, 1097],
        [1098, 1497],
        [1498, 1645],
        [973, 1317],
        [1318, 1579],
        [1580, 1733],
    ]
    for i in range(14):
        dummy.create_constrained_joint(
            type="SPHERICAL", nodes=nodeslist[i], rps=0, damp=0
        )

    nodeslist = [
        [99, 100, 101, 102],
        [227, 228, 229, 230, 231, 232, 233, 234],
        [
            405,
            406,
            407,
            408,
            409,
            410,
            411,
            412,
            413,
            414,
            415,
            416,
            417,
            418,
            419,
            420,
        ],
        [537, 538, 539, 540, 541, 542, 543, 544],
        [603, 604, 605, 606, 607, 608, 609, 610],
        [685, 686, 687, 688],
        [763, 764, 765, 766],
        [865, 866, 867, 868, 869, 870, 871, 872],
        [971, 972, 973, 974, 975, 976, 977, 978, 979, 980, 981, 982],
        [1097, 1098, 1099, 1100, 1101, 1102, 1103, 1104],
        [1317, 1318, 1319, 1320, 1321, 1322, 1323, 1324],
        [1497, 1498, 1499, 1500, 1501, 1502, 1503, 1504],
        [1579, 1580, 1581, 1582, 1583, 1584, 1585, 1586],
        [1645, 1646, 1647, 1648],
        [1733, 1734, 1735, 1736],
    ]

    for i in range(15):
        dummy.create_nodeset(option="LIST", sid=i + 2, entities=nodeslist[i])
        dummy.create_constrained_extra_nodes(option="SET", pid=i + 1, nid=i + 2)

    vector = [0, 0, 0]
    nlist = [
        [100, 229],
        [101, 230],
        [102, 231],
        [232, 409],
        [233, 410],
        [234, 411],
        [412, 867],
        [413, 868],
        [414, 869],
        [870, 974],
        [871, 975],
        [872, 976],
        [415, 539],
        [416, 540],
        [417, 541],
        [542, 686],
        [543, 687],
        [544, 688],
        [418, 605],
        [419, 606],
        [420, 607],
        [608, 764],
        [609, 765],
        [610, 766],
        [977, 1099],
        [978, 1100],
        [979, 1101],
        [1102, 1499],
        [1103, 1500],
        [1104, 1501],
        [1502, 1646],
        [1503, 1647],
        [1504, 1648],
        [980, 1319],
        [981, 1320],
        [982, 1321],
        [1322, 1581],
        [1323, 1582],
        [1324, 1583],
        [1584, 1734],
        [1585, 1735],
        [1586, 1736],
    ]
    for i in range(42):
        id = i + 1
        dummy.create_defineorientation(
            vid=id, iop=2, vector=vector, node1=nlist[i][0], node2=nlist[i][1]
        )

    rolist = [
        4064,
        4190,
        4760,
        2920,
        2920,
        2290,
        2290,
        1350,
        8170,
        2680,
        2680,
        2580,
        2580,
        2000,
        2000,
    ]
    for i in range(15):
        id = i + 1
        e = 4e8
        if id == 6:
            e = 4e6
        dummy.create_mat_rigid(mid=id, ro=rolist[i], e=e, pr=0.3)

    rolist = [4646, 4646, 4646, 4646, 2000, 2000, 4000]
    elist = [4e8, 4e8, 4e9, 4e8, 4.1e8, 4.1e8, 2e8]
    for i in range(16, 23):
        index = i - 16
        dummy.create_mat_elastic(mid=i, ro=rolist[index], e=elist[index], pr=0.3)

    for i in range(1, 43):
        id = 100 + i
        dummy.create_mat_spring_nonlinear_elastic(mid=id, lcid=i)

    dclist = [
        2.3,
        2.3,
        2.3,
        2.3,
        2.3,
        2.3,
        30.5,
        30.5,
        30.5,
        30.5,
        30.5,
        30.5,
        4,
        2,
        4,
        4,
        2,
        2,
        4,
        2,
        4,
        4,
        2,
        2,
        6,
        6,
        5,
        7.5,
        5,
        4,
        1,
        1,
        1,
        6,
        6,
        5,
        7.5,
        5,
        4,
        1,
        1,
        1,
    ]
    for i in range(143, 185):
        index = i - 143
        dummy.create_mat_damper_viscous(mid=i, dc=dclist[index])

    lcidlist = [
        43,
        43,
        43,
        43,
        43,
        43,
        44,
        44,
        45,
        45,
        44,
        44,
        45,
        45,
        46,
        47,
        47,
        48,
        49,
        46,
        47,
        47,
        48,
        49,
    ]
    for i in range(185, 209):
        index = i - 185
        dummy.create_mat_damper_nonlinear_viscous(i, lcidlist[index])

    for i in range(1, 23):
        thk = [0.01, 0.01, 0.01, 0.01]
        nip = 0
        if i == 18:
            thk = [0.1, 0.1, 0.1, 0.1]
        if i == 20 or i == 21:
            nip = 1
        dummy.create_section_shell(secid=i, elform=0, thick=thk, nip=nip)

    dummy.create_section_discrete(secid=101, dro=1)

    for i in range(16, 23):
        dummy.set_partproperty(pid=i, secid=i, mid=i)
    for i in range(101, 209):
        dummy.set_partproperty(pid=i, secid=101, mid=i)
    for i in range(1, 16):
        dummy.set_partproperty(pid=i, secid=i, mid=i)

    dummy.save_file()
