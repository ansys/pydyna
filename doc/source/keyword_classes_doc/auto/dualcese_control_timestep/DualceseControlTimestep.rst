





:class:`DualceseControlTimestep`
================================


.. py:class:: dualcese_control_timestep.DualceseControlTimestep(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_CONTROL_TIMESTEP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseControlTimestep

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~iddt`
            - Get or set the Sets the time step option:
          * - :py:attr:`~cfl`
            - Get or set the CFL number (Courant-Friedrichs-Lewy condition)(0.0 < CFL 1.0)
          * - :py:attr:`~dtint`
            - Get or set the Initial time step size


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

    from dualcese_control_timestep import DualceseControlTimestep

Property detail
---------------

.. py:property:: iddt
   :type: int


   
   Get or set the Sets the time step option:
   Q.0:    Fixed time step size(DTINT, i.e., given initial time step size)
   NE.0 : the time step size will be calculated based on the given CFL - number and the flow solution at the previous time step
















   ..
       !! processed by numpydoc !!

.. py:property:: cfl
   :type: float


   
   Get or set the CFL number (Courant-Friedrichs-Lewy condition)(0.0 < CFL 1.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: dtint
   :type: float


   
   Get or set the Initial time step size
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_TIMESTEP'






