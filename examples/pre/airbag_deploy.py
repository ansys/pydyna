import os

from pydyna.dynaairbag import DynaAirbag

if __name__ == "__main__":
    airbag = DynaAirbag()
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "airbag_deploy" + os.sep
    fns.append(path + "airbag_deploy.k")
    airbag.open_files(fns)

    airbag.create_termination(endtim=0.03)
    airbag.create_control_energy(hgen=2, rwen=2, slnten=2)
    airbag.create_control_output(npopt=1, neecho=3)
    airbag.create_database_binary(dt=5e-4, ieverp=1)
    airbag.create_database_binary(filetype="D3THDT", dt=999999)
    airbag.create_database_ascii(type="ABSTAT", dt=2.0e-4, ioopt=1)
    airbag.create_database_ascii(type="GLSTAT", dt=2.0e-4, ioopt=1)
    airbag.create_database_ascii(type="MATSUM", dt=2.0e-4, ioopt=1)
    airbag.create_database_ascii(type="RCFORC", dt=2.0e-4, ioopt=1)
    airbag.create_database_ascii(type="RBDOUT", dt=2.0e-4, ioopt=1)
    airbag.create_database_ascii(type="RWFORC", dt=2.0e-4, ioopt=1)

    abs = [0, 0.032, 0.045, 0.08]
    ord = [0, 26, 0.6, 0.1]
    airbag.create_definecurve(lcid=1, sfo=1, abscissa=abs, ordinate=ord)

    pids = [3]
    airbag.create_partset(sid=1, pids=pids)
    airbag.create_simple_airbag_model(
        modeltype="SIMPLE_AIRBAG_MODEL",
        sid=1,
        sidtyp=1,
        cv=1.736e3,
        cp=2.43e3,
        t=1.2e3,
        lcid=1,
        mu=0.7,
        area=0,
        pe=14.7,
        ro=3.821e-6,
    )
    tail = [0, 0, 0]
    head = [0, 1, 0]
    airbag.create_rigidwall_planar(nsid=0, tail=tail, head=head, fric=0.5)
    airbag.create_contact(
        cid=1,
        title="airbag and the cylinder",
        option1="NODES_TO_SURFACE",
        option3=False,
        ssid=3,
        msid=2,
        sapr=1,
        sbpr=1,
        fs=0.5,
        fd=0.5,
        sfsb=0.06667,
    )

    airbag.create_mat_rigid(mid=1, ro=7.84e-4, e=30e6, pr=0.3, cmo=1, con1=7, con2=7)
    airbag.create_mat_rigid(mid=2, ro=1.96e-4, e=30e6, pr=0.3)
    airbag.create_mat_fabric(
        mid=3, ro=1e-4, ea=2e6, eb=2e6, prba=0.35, prab=0.35, gab=1.53e6
    )
    thk = [0.5, 0.5, 0.5, 0.5]
    airbag.create_section_shell(secid=1, elform=0, thick=thk)
    thk = [0.015, 0.015, 0.015, 0.015]
    airbag.create_section_shell(secid=2, elform=9, thick=thk, nip=4)
    airbag.set_partproperty(pid=1, secid=1, mid=1)
    airbag.set_partproperty(pid=2, secid=1, mid=2)
    airbag.set_partproperty(pid=3, secid=2, mid=3)
    airbag.save_file()
