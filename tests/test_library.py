"""This module allows pytest to perform unit testing.

Usage:

.. code::

   $ pytest

    ============================= test session starts =============================
    platform linux -- Python 3.8.10, pytest-6.2.5, py-1.10.0, pluggy-1.0.0
    rootdir: /home/alex/ansys/source/template
    plugins: cov-2.12.1
    collected 6 items

    tests/test_library.py ......                                            [100%]

    ============================== 6 passed in 0.04s ==============================


With coverage.

.. code::

   $ pytest --cov ansys.product.library

    ============================= test session starts =============================
    platform linux -- Python 3.8.10, pytest-6.2.5, py-1.10.0, pluggy-1.0.0
    rootdir: /home/alex/ansys/source/template
    plugins: cov-2.12.1
    collected 6 items

    tests/test_library.py ......                                            [100%]

    ---------- coverage: platform linux, python 3.8.10-final-0 -----------
    Name                                    Stmts   Miss  Cover
    -----------------------------------------------------------
    ansys/product/library/__init__.py           2      0   100%
    ansys/product/library/_version.py           2      2     0%
    ansys/product/library/module.py             2      0   100%
    ansys/product/library/other_module.py      59     29    51%
    -----------------------------------------------------------
    TOTAL                                      65     31    52%


    ============================== 6 passed in 0.04s ==============================


"""

import pytest

from ansys.product import library

# this is a fixture that simplifies reuse of common components
@pytest.fixture
def my_complex():
    return library.Complex(1, -2)

@pytest.fixture
def py_complex():
    return 1 - 2j


@pytest.mark.parametrize('a', range(1, 3))
@pytest.mark.parametrize('b', range(1, 4))
def test_add(a, b):
    a = 1
    b = 3
    assert library.add(a, b) == a + b


def test_complex_abs(my_complex, py_complex):
    assert my_complex.abs == abs(py_complex)
