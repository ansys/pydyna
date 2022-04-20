import os

from ansys.dyna.pre.dynaiga import DynaIGA

if __name__ == "__main__":
    iga = DynaIGA()
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep
    fns.append(path + "maino.k")
    fns.append(path + "rkrwelds.key")
    fns.append(path + "27parts.key")
    iga.open_files(fns)

    iga.create_timestep(tssfac=0.9, isdo=3, dt2ms=-0.0004)
    iga.create_termination(endtim=20)
    iga.create_contact(rwpnal=1.0, ignore=1, igactc=1)
    iga.create_database_binary(dt=0.1)

    cylinder1 = [2472.37, -600.000, 1270.98, 2472.37, -600.000, 2668.53, 100, 1000]
    iga.create_rigidwall_geom(
        geomtype=3, motion=0, display=1, parameter=cylinder1, lcid=0, vx=0, vy=0, vz=0
    )

    cylinder2 = [3580.25, -600.000, 1261.37, 3580.25, -600.000, 3130.49, 100, 1000]
    iga.create_rigidwall_geom(
        geomtype=3, motion=0, display=1, parameter=cylinder2, lcid=0, vx=0, vy=0, vz=0
    )

    abs = [0, 100]
    ord = [1, 1]
    iga.create_definecurve(lcid=1, sfo=20, abscissa=abs, ordinate=ord)
    cylinder3 = [3090.59, -955.35, 1299.42, 3090.59, -955.35, 2958.43, 100, 1000]
    iga.create_rigidwall_geom(
        geomtype=3, motion=1, display=1, parameter=cylinder3, lcid=1, vx=0, vy=1, vz=0
    )
    iga.create_section_igashell(secid=1, elform=0, shrf=1.0, thickness=1.0)

    pids2 = []
    for i in range(226, 259):
        if i == 241 or i == 247:
            continue
        iga.set_partproperty(
            pid=i, secid=1, mid=1, eosid=0, hgid=0, grav=0, adpopt=0, tmid=0
        )
        pids2.append(i)
    iga.create_partset(sid=2, pids=pids2)
    iga.create_section_solid(secid=2, elform=1)
    iga.create_hourglass(ghid=1, ihq=1, qm=1, q1=0, q2=0, qb=0, qw=0)

    pids1 = []
    for i in range(1, 100):
        if i == 2:
            iga.set_partproperty(
                pid=i, secid=2, mid=i + 1, eosid=0, hgid=0, grav=0, adpopt=0, tmid=0
            )
        iga.set_partproperty(
            pid=i, secid=2, mid=i + 1, eosid=0, hgid=1, grav=0, adpopt=0, tmid=0
        )
        pids1.append(i)
    iga.create_partset(sid=1, pids=pids1)
    iga.create_contact_automatic(ssid=2, msid=0, sstyp=2, mstyp=0, option=1)
    iga.create_contact_tied(ssid=1, msid=2, sstyp=2, mstyp=2)
    elements = iga.get_solid_elements()
    iga.save_file()

    
