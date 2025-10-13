





:class:`ControlSteadyStateRolling`
==================================


.. py:class:: control_steady_state_rolling.ControlSteadyStateRolling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_STEADY_STATE_ROLLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSteadyStateRolling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~imass`
            - Get or set the Inertia switching flag
          * - :py:attr:`~lcdmu`
            - Get or set the Optional load curve for scaling the friction forces in contact.
          * - :py:attr:`~lcdmur`
            - Get or set the Optional load curve for scaling the friction forces in contact during dynamic relaxation. If  LCDMUR isn’t specified, LCDMU is used.
          * - :py:attr:`~ivel`
            - Get or set the Velocity switching flag.
          * - :py:attr:`~scl_k`
            - Get or set the Scale factor for the friction stiffness during contact loading and unloading. The default values are 1.0 and 0.01 for explicit and implicit, respectively. Any scaling applied here applies only to contact involving the subsystem of parts defined for steady state rolling.


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

    from control_steady_state_rolling import ControlSteadyStateRolling

Property detail
---------------

.. py:property:: imass
   :type: int


   
   Get or set the Inertia switching flag
   EQ.0:  include inertia during an implicit dynamic simulation.
   EQ.1:  treat steady state rolling subsystems as quasi-static during implicit dynamic simulations.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdmu
   :type: int


   
   Get or set the Optional load curve for scaling the friction forces in contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdmur
   :type: int


   
   Get or set the Optional load curve for scaling the friction forces in contact during dynamic relaxation. If  LCDMUR isn’t specified, LCDMU is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivel
   :type: int


   
   Get or set the Velocity switching flag.
   EQ.0:  eliminate the steady state rolling body forces and set the velocities of the nodes after dynamic relaxation.
   EQ.1:  keep the steady state rolling body forces after dynamic relaxation instead of setting the velocities.
















   ..
       !! processed by numpydoc !!

.. py:property:: scl_k
   :type: int


   
   Get or set the Scale factor for the friction stiffness during contact loading and unloading. The default values are 1.0 and 0.01 for explicit and implicit, respectively. Any scaling applied here applies only to contact involving the subsystem of parts defined for steady state rolling.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'STEADY_STATE_ROLLING'






