





:class:`EmRandlesBatmac`
========================


.. py:class:: em_randles_batmac.EmRandlesBatmac(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_RANDLES_BATMAC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmRandlesBatmac

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rdlid`
            - Get or set the Id of the Randles Cell.
          * - :py:attr:`~rdltype`
            - Get or set the Type of Randles Cell
          * - :py:attr:`~rdlarea`
            - Get or set the Randle Area:
          * - :py:attr:`~psid`
            - Get or set the Part Set ID of all the parts composing the cell.
          * - :py:attr:`~q`
            - Get or set the Cell capacity.
          * - :py:attr:`~cq`
            - Get or set the SOC conversion factor (%/s), known to be equal to 1/36 in S.I units.
          * - :py:attr:`~socinit`
            - Get or set the Initial state of charge of the cell.
          * - :py:attr:`~soctou`
            - Get or set the Equilibrium voltage (OCV):
          * - :py:attr:`~r0cha`
            - Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
          * - :py:attr:`~r0dis`
            - Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
          * - :py:attr:`~r10cha`
            - Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
          * - :py:attr:`~r10dis`
            - Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
          * - :py:attr:`~c10cha`
            - Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
          * - :py:attr:`~c10dis`
            - Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
          * - :py:attr:`~r20cha`
            - Get or set the r20 when the current flows in the charge direction:
          * - :py:attr:`~r20dis`
            - Get or set the r20 when the current flows in the discharge direction:
          * - :py:attr:`~c20cha`
            - Get or set the c20 when the current flows in the charge direction:
          * - :py:attr:`~c20dis`
            - Get or set the c20 when the current flows in the discharge direction:
          * - :py:attr:`~r30cha`
            - Get or set the r30 when the current flows in the charge direction:
          * - :py:attr:`~r30dis`
            - Get or set the r30 when the current flows in the discharge direction:
          * - :py:attr:`~c30cha`
            - Get or set the c30 when the current flows in the charge direction:
          * - :py:attr:`~c30dis`
            - Get or set the c30 when the current flows in the discharge direction:
          * - :py:attr:`~temp`
            - Get or set the Constant temperature value used for the Randles circuit parameters in case there is no coupling with the thermal solver.
          * - :py:attr:`~frther`
            - Get or set the From Thermal:
          * - :py:attr:`~r0toth`
            - Get or set the R0 to Thermal:
          * - :py:attr:`~dudt`
            - Get or set the If negative integer, load curve ID of the reversible heat as a function of SOC.
          * - :py:attr:`~tempu`
            - Get or set the Temperature Unit :
          * - :py:attr:`~usesocs`
            - Get or set the Use SOC shift:
          * - :py:attr:`~tau`
            - Get or set the Damping time in the SOCshift equation.
          * - :py:attr:`~flcid`
            - Get or set the Load curve giving f(i) where I is the total current in the unit cell.


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

    from em_randles_batmac import EmRandlesBatmac

Property detail
---------------

.. py:property:: rdlid
   :type: Optional[int]


   
   Get or set the Id of the Randles Cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdltype
   :type: int


   
   Get or set the Type of Randles Cell
   EQ.-1:User defined equivalent circuit model
   EQ.0:0-order Randles Cell.
   EQ.1:1-order Randles Cell.
   EQ.2:2-order Randles Cell
   EQ.3:3-order Randles Cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdlarea
   :type: int


   
   Get or set the Randle Area:
   EQ.1: The parameters are per unit area and will be scaled in each Randle circuit by a factor depending on the local area of the circuit.
   EQ.2: Default. The parameters are defined for the whole cell and will be scaled in each Randle circuit by a factor depending on the local area of the circuit and the global area of the cell.
   EQ.3:The parameters are not scaled by area factors.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part Set ID of all the parts composing the cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Cell capacity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cq
   :type: Optional[float]


   
   Get or set the SOC conversion factor (%/s), known to be equal to 1/36 in S.I units.
















   ..
       !! processed by numpydoc !!

.. py:property:: socinit
   :type: Optional[float]


   
   Get or set the Initial state of charge of the cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: soctou
   :type: Optional[float]


   
   Get or set the Equilibrium voltage (OCV):
   GE.0.0: constant value
   LT.0.0: |SOCTOU| is a load curve ID defining equilibrium voltage(OCV) as a function of the state of charge (SOC).
















   ..
       !! processed by numpydoc !!

.. py:property:: r0cha
   :type: Optional[float]


   
   Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
















   ..
       !! processed by numpydoc !!

.. py:property:: r0dis
   :type: Optional[float]


   
   Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
















   ..
       !! processed by numpydoc !!

.. py:property:: r10cha
   :type: Optional[float]


   
   Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
















   ..
       !! processed by numpydoc !!

.. py:property:: r10dis
   :type: Optional[float]


   
   Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
















   ..
       !! processed by numpydoc !!

.. py:property:: c10cha
   :type: Optional[float]


   
   Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
















   ..
       !! processed by numpydoc !!

.. py:property:: c10dis
   :type: Optional[float]


   
   Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
















   ..
       !! processed by numpydoc !!

.. py:property:: r20cha
   :type: Optional[float]


   
   Get or set the r20 when the current flows in the charge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: r20dis
   :type: Optional[float]


   
   Get or set the r20 when the current flows in the discharge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c20cha
   :type: Optional[float]


   
   Get or set the c20 when the current flows in the charge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c20dis
   :type: Optional[float]


   
   Get or set the c20 when the current flows in the discharge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: r30cha
   :type: Optional[float]


   
   Get or set the r30 when the current flows in the charge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: r30dis
   :type: Optional[float]


   
   Get or set the r30 when the current flows in the discharge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c30cha
   :type: Optional[float]


   
   Get or set the c30 when the current flows in the charge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: c30dis
   :type: Optional[float]


   
   Get or set the c30 when the current flows in the discharge direction:
   GE.0.0:constant value.
   LT.0.0:absolute value is a define function or table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: float


   
   Get or set the Constant temperature value used for the Randles circuit parameters in case there is no coupling with the thermal solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: frther
   :type: int


   
   Get or set the From Thermal:
   EQ.0:The temperature used in the Randles circuit parameters is TEMP.
   EQ.1: The temperature used in the Randles circuit parameter is the temperature from the thermal solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: r0toth
   :type: int


   
   Get or set the R0 to Thermal:
   EQ.0:The joule heating in the resistance r0 is not added to the thermal solver.
   EQ.1:The joule heating in the resistance r0 is added to the thermal solver.
















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
   EQ.0:The temperature is in Celsius
   EQ.1:The Temperature is in Kelvin.
















   ..
       !! processed by numpydoc !!

.. py:property:: usesocs
   :type: int


   
   Get or set the Use SOC shift:
   EQ.0:Don't use the added SOCshift
   EQ.1:Use the added SOCshift.
















   ..
       !! processed by numpydoc !!

.. py:property:: tau
   :type: Optional[float]


   
   Get or set the Damping time in the SOCshift equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: flcid
   :type: Optional[int]


   
   Get or set the Load curve giving f(i) where I is the total current in the unit cell.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'RANDLES_BATMAC'






