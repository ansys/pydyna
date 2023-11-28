PyDYNA
======

.. readme_start

|pyansys| |python| |pypi| |GH-CI| |codecov| |MIT| |black|

.. |pyansys| image:: https://img.shields.io/badge/Py-Ansys-ffc107.svg?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAABDklEQVQ4jWNgoDfg5mD8vE7q/3bpVyskbW0sMRUwofHD7Dh5OBkZGBgW7/3W2tZpa2tLQEOyOzeEsfumlK2tbVpaGj4N6jIs1lpsDAwMJ278sveMY2BgCA0NFRISwqkhyQ1q/Nyd3zg4OBgYGNjZ2ePi4rB5loGBhZnhxTLJ/9ulv26Q4uVk1NXV/f///////69du4Zdg78lx//t0v+3S88rFISInD59GqIH2esIJ8G9O2/XVwhjzpw5EAam1xkkBJn/bJX+v1365hxxuCAfH9+3b9/+////48cPuNehNsS7cDEzMTAwMMzb+Q2u4dOnT2vWrMHu9ZtzxP9vl/69RVpCkBlZ3N7enoDXBwEAAA+YYitOilMVAAAAAElFTkSuQmCC
   :target: https://docs.pyansys.com/
   :alt: PyAnsys

.. |python| image:: https://img.shields.io/pypi/pyversions/ansys-dyna-core?logo=pypi
   :target: https://pypi.org/project/ansys-dyna-core/
   :alt: Python

.. |pypi| image:: https://img.shields.io/pypi/v/ansys-dyna-core.svg?logo=python&logoColor=white
   :target: https://pypi.org/project/ansys-dyna-core
   :alt: PyPI

.. |codecov| image:: https://codecov.io/gh/ansys/ansys-dyna-core/branch/main/graph/badge.svg
   :target: https://codecov.io/gh/ansys/pydyna
   :alt: Codecov

.. |GH-CI| image:: https://github.com/ansys/pydyna/actions/workflows/ci_cd.yml/badge.svg
   :target: https://github.com/ansys/pydyna/actions/workflows/ci_cd.yml
   :alt: GH-CI

.. |MIT| image:: https://img.shields.io/badge/License-MIT-yellow.svg
   :target: https://opensource.org/licenses/MIT
   :alt: MIT

.. |black| image:: https://img.shields.io/badge/code%20style-black-000000.svg?style=flat
   :target: https://github.com/psf/black
   :alt: Black

Overview
========
PyDYNA is a Pythonic package for providing a more convenient and complete way to
build an Ansys DYNA input deck, submit it to the Ansys LS-DYNA solver, and
finally postprocess the results. 

PyDYNA contains two submodules, ``ansys.dyna.core.pre`` and ``ansys.dyna.core.solver``

- ``pre``: This module provides highly abstracted APIs for creating and
  setting up DYNA input decks. There are many classes supported, namely,
  DynaMech, DynaIGA, DynaICFD, DynaSALE, DynaEM,DynaNVH, DynaMaterial,
  DynaISPH, DynaICFD and DynaAirbag. Each of these classes can be used to generate
  LS-DYNA keywords. Since these classes have high-level abstraction, each function call
  generates groups of keywords needed to define an input in LS-DYNA.
- ``solver``: This API provides features to interact directly with the Ansys LS-DYNA solver.
  LS-DYNA is primarily a batch solver with very limited interactive capabilities, the
  ``solver`` service provides a way to push input files to the LS-DYNA solver, monitor the state
  of the running job, change the value of a load curve and finally retrieve result files back from
  the server

Once you have results, you can use the Ansys Data Processing Framework (DPF),
which is designed to provide numerical simulation users and engineers
with a toolbox for accessing and transforming simulation data. DPF
can access data from Ansys solver files and from several files with neutral formats,
including CSV, HDF5, and VTK. Using DPF's various operators,
you can manipulate and transform this data.

The `ansys-dpf-post package <https://github.com/ansys/pydpf-post>`_ provides
a simplified Python interface to DPF, thus enabling rapid postprocessing
without ever leaving a Python environment. For more information on DPF-Post,
see the `DPF-Post documentation <https://post.docs.pyansys.com>`_.

Documentation and issues
========================
Documentation for the latest stable release of PyDyna is hosted at `PyDYNA documentation
<https://dyna.docs.pyansys.com/version/stable//>`_.

For examples on how to use PyDYNA, see `Examples <https://dyna.docs.pyansys.com/version/stable/examples/index.html>`_
in the PyDYNA documentation.

In the upper right corner of the documentation's title bar, there is an option for switching from
viewing the documentation for the latest stable release to viewing the documentation for the
development version or previously released versions.

On the `PyDYNA Issues <https://github.com/ansys/pydyna/issues>`_ page, you can create issues to
report bugs and request new features. On the `PyDYNA Discussions <https://github.com/ansys/pydyna/discussions>`_
page or the `Discussions <https://discuss.ansys.com/>`_ page on the Ansys Developer portal,
you can post questions, share ideas, and get community feedback. 

To reach the project support team, email `pyansys.core@ansys.com <pyansys.core@ansys.com>`_.

License
=======
PyDYNA is licensed under the MIT license.

PyDYNA makes no commercial claim over Ansys whatsoever. This library extends the functionality of
Ansys LS-DYNA by adding a Python interface to LS-DYNA without changing the core behavior or
license of the original software. The use of the interactive control of PyDYNA requires a legally
licensed local copy of LS-DYNA.

For more information on LS-DYNA, see the
`Ansys LS-DYNA <https://www.ansys.com/products/structures/ansys-ls-dyna>`_
page on the Ansys website.

.. LINKS AND REFERENCES
.. _pip: https://pypi.org/project/pip/
.. _PyAnsys Developer's Guide: https://dev.docs.pyansys.com/
