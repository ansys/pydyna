





:class:`ChangeThermalParameters`
================================


.. py:class:: change_thermal_parameters.ChangeThermalParameters(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHANGE_THERMAL_PARAMETERS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChangeThermalParameters

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ts`
            - Get or set the Thermal time step code:
          * - :py:attr:`~dt`
            - Get or set the Thermal time step on restart:
          * - :py:attr:`~tmin`
            - Get or set the Minimum thermal timestep:
          * - :py:attr:`~tmax`
            - Get or set the Maximum thermal timestep:
          * - :py:attr:`~dtemp`
            - Get or set the Maximum temperature change in a thermal timestep:
          * - :py:attr:`~tscp`
            - Get or set the Time step control parameter (0.0 < TSCP < 1.0 ):
          * - :py:attr:`~refmax`
            - Get or set the Maximum number of reformations per thermal time step:
          * - :py:attr:`~tol`
            - Get or set the Non-linear convergence tolerance:


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

    from change_thermal_parameters import ChangeThermalParameters

Property detail
---------------

.. py:property:: ts
   :type: int


   
   Get or set the Thermal time step code:
   EQ.0: No change (default),
   EQ.1: Fixed timestep,
   EQ.2: variable timestep.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Thermal time step on restart:
   EQ.0.0: No change (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tmin
   :type: float


   
   Get or set the Minimum thermal timestep:
   EQ.0.0: No change (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tmax
   :type: float


   
   Get or set the Maximum thermal timestep:
   EQ.0.0: No change (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: dtemp
   :type: float


   
   Get or set the Maximum temperature change in a thermal timestep:
   EQ.0.0: No change (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tscp
   :type: float


   
   Get or set the Time step control parameter (0.0 < TSCP < 1.0 ):
   EQ.0.0: No change (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: refmax
   :type: int


   
   Get or set the Maximum number of reformations per thermal time step:
   EQ.0: No change (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the Non-linear convergence tolerance:
   EQ.0.0: No change (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHANGE'


.. py:attribute:: subkeyword
   :value: 'THERMAL_PARAMETERS'






