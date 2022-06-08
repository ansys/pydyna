import os

from pydyna.dynaicfd import DynaICFD

if __name__ == "__main__":
    icfd = DynaICFD()
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep
    fns.append(path + "mesh.k")
    icfd.open_files(fns)

    icfd.create_control_time(tim=100, dt=0)
    icfd.create_section_icfd(sid=1)
    icfd.create_mat_icfd(mid=1, flg=1, ro=1.0, vis=0.005)
    icfd.create_part_icfd(pid=1, secid=1, mid=1)
    icfd.create_part_icfd(pid=2, secid=1, mid=1)
    icfd.create_part_icfd(pid=3, secid=1, mid=1)
    icfd.create_part_icfd(pid=4, secid=1, mid=1)

    spids = [1, 2, 3, 4]

    icfd.create_part_vol(pid=10, secid=1, mid=1, spids=spids)
    icfd.create_db_drag(pid=4)

    abs = [0, 10000]
    ord = [1, 1]
    icfd.create_definecurve(lcid=1, sfo=1, abscissa=abs, ordinate=ord)

    abs = [0, 10000]
    ord = [0, 0]
    icfd.create_definecurve(lcid=2, sfo=1, abscissa=abs, ordinate=ord)

    icfd.create_bdy_prescribed_vel(pid=1, dof=1, vad=1, lcid=1)
    icfd.create_bdy_prescribed_vel(pid=1, dof=2, vad=1, lcid=2)
    icfd.create_bdy_prescribed_pre(pid=2, lcid=2)
    icfd.create_bdy_free_slip(pid=3)
    icfd.create_bdy_non_slip(pid=4)

    icfd.mesh_create_volume(volid=1, pids=spids)
    icfd.mesh_create_bl(pid=4, nelth=2)
    icfd.create_database_binary(dt=1)

    icfd.save_file()
