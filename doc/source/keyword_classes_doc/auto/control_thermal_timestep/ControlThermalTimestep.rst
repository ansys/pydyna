





:class:`ControlThermalTimestep`
===============================


.. py:class:: control_thermal_timestep.ControlThermalTimestep(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_THERMAL_TIMESTEP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlThermalTimestep

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ts`
            - Get or set the Time step control:
          * - :py:attr:`~tip`
            - Get or set the Time integration parameter:
          * - :py:attr:`~its`
            - Get or set the Initial thermal time step
          * - :py:attr:`~tmin`
            - Get or set the Minimum thermal time step:
          * - :py:attr:`~tmax`
            - Get or set the Maximum thermal time step:
          * - :py:attr:`~dtemp`
            - Get or set the Maximum temperature change in each time step above which the thermal timestep will be decreased
          * - :py:attr:`~tscp`
            - Get or set the Time step control parameter. The thermal time step is decreased by this factor if convergence is not obtained. 0.0 < TSCP < 1.0:
          * - :py:attr:`~lcts`
            - Get or set the LCTS designates a load curve number which defines the thermal time step as a function of time. If LCTS is defined, then the other time step control parameters on this keyword are ignored.


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

    from control_thermal_timestep import ControlThermalTimestep

Property detail
---------------

.. py:property:: ts
   :type: int


   
   Get or set the Time step control:
   EQ.0: fixed time step (default),
   EQ.1: variable time step (may increase or decrease).
















   ..
       !! processed by numpydoc !!

.. py:property:: tip
   :type: float


   
   Get or set the Time integration parameter:
   Default is 0.5 - Crank-Nicholson scheme (default),
   EQ 1.0: fully implicit.
















   ..
       !! processed by numpydoc !!

.. py:property:: its
   :type: Optional[float]


   
   Get or set the Initial thermal time step
















   ..
       !! processed by numpydoc !!

.. py:property:: tmin
   :type: Optional[float]


   
   Get or set the Minimum thermal time step:
   EQ.0.0. Set to structural explicit timestep.
   LT.0.0: curve ID=(-TMIN) gives minimum thermal time step size as function of time.The load curve defines pairs(thermal time breakpoint, new minimum time step).
















   ..
       !! processed by numpydoc !!

.. py:property:: tmax
   :type: Optional[float]


   
   Get or set the Maximum thermal time step:
   EQ.0.0: Set to 100 * structural explicit timestep.
   LT.0.0: curve ID=(-TMAX) gives maximum thermal time step size as function of time.The load curve defines pairs(thermal time breakpoint,new maximum time step).The time step will be adjusted to hit the time breakpoints exactly.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtemp
   :type: float


   
   Get or set the Maximum temperature change in each time step above which the thermal timestep will be decreased
   EQ.0.0: set to a temperature change of 1.0.
   LT.0.0: curve ID=(-DTEMP) gives maximum temperature change as function of time.The load curve defines pairs(thermal time breakpoint,new temperature change).
















   ..
       !! processed by numpydoc !!

.. py:property:: tscp
   :type: float


   
   Get or set the Time step control parameter. The thermal time step is decreased by this factor if convergence is not obtained. 0.0 < TSCP < 1.0:
   Default value is 0.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcts
   :type: Optional[int]


   
   Get or set the LCTS designates a load curve number which defines the thermal time step as a function of time. If LCTS is defined, then the other time step control parameters on this keyword are ignored.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'THERMAL_TIMESTEP'






