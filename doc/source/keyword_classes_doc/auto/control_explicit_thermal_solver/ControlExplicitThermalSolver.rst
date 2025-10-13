





:class:`ControlExplicitThermalSolver`
=====================================


.. py:class:: control_explicit_thermal_solver.ControlExplicitThermalSolver(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_EXPLICIT_THERMAL_SOLVER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlExplicitThermalSolver

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~partset`
            - Get or set the Part set ID (See *SET_PART).
          * - :py:attr:`~dtfac`
            - Get or set the Time step factor (see Remark 1).


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from control_explicit_thermal_solver import ControlExplicitThermalSolver

Property detail
---------------

.. py:property:: partset
   :type: Optional[int]


   
   Get or set the Part set ID (See *SET_PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: dtfac
   :type: float


   
   Get or set the Time step factor (see Remark 1).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'EXPLICIT_THERMAL_SOLVER'






