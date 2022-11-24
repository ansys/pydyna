"""Installation file for ansys.dyna.solver"""
import os
from io import open as io_open

from setuptools import setup, find_namespace_packages


# Get version from version info
__version__ = None
this_file = os.path.dirname(__file__)
version_file = os.path.join(this_file, "ansys", "dyna", "solver", "_version.py")
with io_open(version_file, mode="r") as fd:
    # execute file from raw string
    exec(fd.read())

install_requires = [
    "grpcio>=1.30.0",
    "grpcio-tools>= 1.39.0",
    "protobuf~=3.19",
    "google-api-python-client>=1.7.11",
    "googleapis-common-protos>=1.52.0",
]


packages = []
for package in find_namespace_packages(include="ansys*"):
    if package.startswith("ansys.dyna"):
        packages.append(package)


setup(
    name="ansys-dyna-solver",
    packages=packages,
    version=__version__,
    description="Python interface to LSDYNA Service",
    long_description=open("README.rst").read(),
    license="MIT",
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering :: Information Analysis",
        "License :: OSI Approved :: MIT License",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: POSIX",
        "Operating System :: MacOS",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
    ],
    url="https://github.com/pyansys/pyDyna",
    python_requires=">=3.7",
    keywords=["ANSYS", "LS-DYNA", "gRPC"],
#    package_data={
#       "":["*.proto","Makefile","client.py","runjob_example.py"],
#    },
    install_requires=install_requires,
)
