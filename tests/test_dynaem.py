import os


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynaem import DynaEM, BEMSOLVER, FEMSOLVER, EMContact


def comparefile(outputf, standardf):
    with open(outputf, "r") as fp1, open(standardf, "r") as fp2:
        line = fp1.readline()
        line = fp1.readline()
        while True:
            line1 = fp1.readline()
            line2 = fp2.readline()
            if line1 == "" or line2 == "":
                break
            if line1 != line2:
                return False
    return True


def test_em(em_initialfile, resolve_server_path, resolve_standard_path):
    solution = DynaSolution("localhost")
    fns = []
    fns.append(em_initialfile)
    solution.open_files(fns)
    em = DynaEM()
    solution.add(em)
    em.analysis.set_timestep(timestep=5e-6)
    em.analysis.set_solver_bem(solver=BEMSOLVER.PCG)
    em.analysis.set_solver_fem(solver=FEMSOLVER.DIRECT_SOLVER, relative_tol=1e-3)
    contact = EMContact()
    em.contacts.add(contact)
    em.create_em_output(mats=2, matf=2, sols=2, solf=2)
    em.create_em_database_globalenergy(outlv=1)
    solution.save_file()
    outputfile = os.path.join(resolve_server_path, "output", "test_em.k")
    standardfile = os.path.join(resolve_standard_path, "em.k")
    assert comparefile(outputfile, standardfile)
