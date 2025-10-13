





:class:`EmBatteryRandles`
=========================


.. py:class:: em_battery_randles.EmBatteryRandles(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_BATTERY_RANDLES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmBatteryRandles

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rdlid`
            - Get or set the Id of the Randle Cell
          * - :py:attr:`~rdltype`
            - Get or set the Type of Randle Cell
          * - :py:attr:`~rdlarea`
            - Get or set the Randle Area:
          * - :py:attr:`~ccppart`
            - Get or set the CCP Part ID.
          * - :py:attr:`~ccnpart`
            - Get or set the CCN Part ID.
          * - :py:attr:`~seppart`
            - Get or set the Separator Part ID
          * - :py:attr:`~poselpart`
            - Get or set the Positive Electrode Part ID
          * - :py:attr:`~negelpart`
            - Get or set the Negative Electrode Part ID
          * - :py:attr:`~q`
            - Get or set the Unit cell capacity
          * - :py:attr:`~cq`
            - Get or set the SOC conversion factor (%/s), known to be equal to 1/36 in S.I units.
          * - :py:attr:`~socinit`
            - Get or set the Initial state of charge of the unit cell.
          * - :py:attr:`~soctou`
            - Get or set the Constant value if positive or load curve ID if negative integer defining the equilibrium voltage (OCV) as a function of the state of charge (SOC).
          * - :py:attr:`~r0cha`
            - Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
          * - :py:attr:`~r0dis`
            - Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
          * - :py:attr:`~r10cha`
            - Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
          * - :py:attr:`~r10dis`
            - Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
          * - :py:attr:`~c10cha`
            - Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
          * - :py:attr:`~c10dis`
            - Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
          * - :py:attr:`~temp`
            - Get or set the Constant temperature value used for the Randle circuit parameters in case there is no coupling with the thermal solver (FRTHERM =0)
          * - :py:attr:`~frtherm`
            - Get or set the From Thermal :
          * - :py:attr:`~r0toth`
            - Get or set the R0 to Thermal :
          * - :py:attr:`~dudt`
            - Get or set the If negative integer, load curve ID of the reversible heat as a function of SOC.
          * - :py:attr:`~tempu`
            - Get or set the Temperature Unit :
          * - :py:attr:`~usesocs`
            - Get or set the Use SOC shift  :
          * - :py:attr:`~tausocs`
            - Get or set the Damping time in the SOCshift equation
          * - :py:attr:`~sicslcid`
            - Get or set the Load curve giving f(i) where I is the total current in the unit cell


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

    from em_battery_randles import EmBatteryRandles

Property detail
---------------

.. py:property:: rdlid
   :type: Optional[int]


   
   Get or set the Id of the Randle Cell
















   ..
       !! processed by numpydoc !!

.. py:property:: rdltype
   :type: Optional[int]


   
   Get or set the Type of Randle Cell
















   ..
       !! processed by numpydoc !!

.. py:property:: rdlarea
   :type: int


   
   Get or set the Randle Area:
   EQ.0:   Default.The parameters are not scaled by area factors.
   EQ.1:   The parameters are per unit area and will be scaled in each Randle circuit by a factor depending on the local area of the circuit.
   EQ.2:   The parameters are defined for the whole unit cell and will be scaled in each Randle circuit by a factor depending on the local area of the circuit and the global area of the cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: ccppart
   :type: Optional[int]


   
   Get or set the CCP Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ccnpart
   :type: Optional[int]


   
   Get or set the CCN Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: seppart
   :type: Optional[int]


   
   Get or set the Separator Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: poselpart
   :type: Optional[int]


   
   Get or set the Positive Electrode Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: negelpart
   :type: Optional[int]


   
   Get or set the Negative Electrode Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Unit cell capacity
















   ..
       !! processed by numpydoc !!

.. py:property:: cq
   :type: Optional[float]


   
   Get or set the SOC conversion factor (%/s), known to be equal to 1/36 in S.I units.
















   ..
       !! processed by numpydoc !!

.. py:property:: socinit
   :type: Optional[float]


   
   Get or set the Initial state of charge of the unit cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: soctou
   :type: Optional[float]


   
   Get or set the Constant value if positive or load curve ID if negative integer defining the equilibrium voltage (OCV) as a function of the state of charge (SOC).
















   ..
       !! processed by numpydoc !!

.. py:property:: r0cha
   :type: Optional[float]


   
   Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
   -SOC if load curve
   -SOC and Temperature if table.
















   ..
       !! processed by numpydoc !!

.. py:property:: r0dis
   :type: Optional[float]


   
   Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
   -SOC if load curve
   -SOC and Temperature if table.
















   ..
       !! processed by numpydoc !!

.. py:property:: r10cha
   :type: Optional[float]


   
   Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
   -SOC if load curve
   -SOC and Temperature if table.
















   ..
       !! processed by numpydoc !!

.. py:property:: r10dis
   :type: Optional[float]


   
   Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
   -SOC if load curve
   -SOC and Temperature if table.
















   ..
       !! processed by numpydoc !!

.. py:property:: c10cha
   :type: Optional[float]


   
   Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
   -SOC if load curve
   -SOC and Temperature if table.
















   ..
       !! processed by numpydoc !!

.. py:property:: c10dis
   :type: Optional[float]


   
   Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
   -SOC if load curve
   -SOC and Temperature if table.
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: Optional[float]


   
   Get or set the Constant temperature value used for the Randle circuit parameters in case there is no coupling with the thermal solver (FRTHERM =0)
















   ..
       !! processed by numpydoc !!

.. py:property:: frtherm
   :type: int


   
   Get or set the From Thermal :
   EQ.0:   The temperature used in the Randle circuit parameters is TEMP
   EQ.1:   The temperature used in the Randle circuit parameter is the temperature from the thermal solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: r0toth
   :type: int


   
   Get or set the R0 to Thermal :
   EQ.0:   The joule heating in the resistance r0 is not added to the thermal solver
   EQ.1:   The joule heating in the resistance r0 is added to the thermal solver
















   ..
       !! processed by numpydoc !!

.. py:property:: dudt
   :type: Optional[float]


   
   Get or set the If negative integer, load curve ID of the reversible heat as a function of SOC.
















   ..
       !! processed by numpydoc !!

.. py:property:: tempu
   :type: int


   
   Get or set the Temperature Unit :
   EQ.0:   The temperature is in Celsius
   EQ.1:   The Temperature is in Kelvin
















   ..
       !! processed by numpydoc !!

.. py:property:: usesocs
   :type: int


   
   Get or set the Use SOC shift  :
   EQ.0:   Don't use the added SOCshift
   EQ.1:   Use the added SOCshift
















   ..
       !! processed by numpydoc !!

.. py:property:: tausocs
   :type: Optional[float]


   
   Get or set the Damping time in the SOCshift equation
















   ..
       !! processed by numpydoc !!

.. py:property:: sicslcid
   :type: Optional[int]


   
   Get or set the Load curve giving f(i) where I is the total current in the unit cell
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'BATTERY_RANDLES'






