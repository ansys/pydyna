





:class:`ControlThermalNonlinear`
================================


.. py:class:: control_thermal_nonlinear.ControlThermalNonlinear(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_THERMAL_NONLINEAR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlThermalNonlinear

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~refmax`
            - Get or set the Maximum number of matrix reformations per time step (default = 10)
          * - :py:attr:`~tol`
            - Get or set the Convergence tolerance for temperature: EQ.0.0: set to 1000 * machine roundoff.
          * - :py:attr:`~dcp`
            - Get or set the Divergence control parameter:
          * - :py:attr:`~lumpbc`
            - Get or set the Lump enclosure radiation boundary condition:
          * - :py:attr:`~thlstl`
            - Get or set the Line search convergence tolerance:
          * - :py:attr:`~nlthpr`
            - Get or set the Thermal nonlinear print out level:
          * - :py:attr:`~phchpn`
            - Get or set the Phase change penalty parameter:


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

    from control_thermal_nonlinear import ControlThermalNonlinear

Property detail
---------------

.. py:property:: refmax
   :type: int


   
   Get or set the Maximum number of matrix reformations per time step (default = 10)
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the Convergence tolerance for temperature: EQ.0.0: set to 1000 * machine roundoff.
















   ..
       !! processed by numpydoc !!

.. py:property:: dcp
   :type: float


   
   Get or set the Divergence control parameter:
   steady state problems 0.3 <= DCP<=1.0.   default is 1.0
   transient problems 0.0 < DCP <=1.0.    default is 0.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: lumpbc
   :type: int


   
   Get or set the Lump enclosure radiation boundary condition:
   EQ.0: off (default)
   EQ.1: on
















   ..
       !! processed by numpydoc !!

.. py:property:: thlstl
   :type: float


   
   Get or set the Line search convergence tolerance:
   EQ.0.0: No line search
   GT.0.0: Line search convergence tolerance
















   ..
       !! processed by numpydoc !!

.. py:property:: nlthpr
   :type: int


   
   Get or set the Thermal nonlinear print out level:
   EQ.0: No print out
   EQ.1: Print convergence parameters during solution of nonlinear system
















   ..
       !! processed by numpydoc !!

.. py:property:: phchpn
   :type: float


   
   Get or set the Phase change penalty parameter:
   EQ.0.0: Penalty formulation not activated
   GT.0.0: Penalty to enforce constant phase change temperature
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'THERMAL_NONLINEAR'






