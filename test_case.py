import pathlib

from ansys.dyna.core.run import run_dyna

this_path = pathlib.Path(__file__).parent.resolve()
test_file = this_path / "tests"/ "testfiles"/ "run"/ "case-keywords"/ "i.k"
print(f"Running test case: {test_file}")
example_folder = str(test_file.parent.resolve())
try:
    wdir = run_dyna(str(test_file), working_directory=example_folder, activate_case=True)
except Exception as e:
    print(f"Error running test case: {e}")
    wdir = None