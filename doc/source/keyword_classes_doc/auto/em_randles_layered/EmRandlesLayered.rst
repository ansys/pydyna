





:class:`EmRandlesLayered`
=========================


.. py:class:: em_randles_layered.EmRandlesLayered(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_RANDLES_LAYERED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmRandlesLayered

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
          * - :py:attr:`~psid`
            - Get or set the Part Set ID of all the parts composing the cell.
          * - :py:attr:`~rdlarea_`
            - Get or set the Randle Area:
          * - :py:attr:`~q`
            - Get or set the Cell capacity.
          * - :py:attr:`~cq`
            - Get or set the SOC conversion factor (%/s), known to be equal to 1/36 in S.I units.
          * - :py:attr:`~socinit`
            - Get or set the Initial state of charge of the cell.
          * - :py:attr:`~soctou`
            - Get or set the Constant value if positive or load curve ID if negative integer defining the equilibrium voltage (OCV) as a function of the state of charge (SOC).
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
          * - :py:attr:`~temp`
            - Get or set the Constant temperature value used for the Randles circuit parameters in case there is no coupling with the thermal solver.


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

    from em_randles_layered import EmRandlesLayered

Property detail
---------------

.. py:property:: rdlid
   :type: Optional[int]


   
   Get or set the Id of the Randles Cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdltype
   :type: Optional[int]


   
   Get or set the Type of Randles Cell
   EQ.1:   Only option available for now.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part Set ID of all the parts composing the cell.
















   ..
       !! processed by numpydoc !!

.. py:property:: rdlarea_
   :type: Optional[int]


   
   Get or set the Randle Area:
   EQ.0:   Default.The parameters are not scaled by area factors.
   EQ.1:   The parameters are per unit area and will be scaled in each Randle circuit by a factor depending on the local area of the circuit.
   EQ.2:   The parameters are defined for the whole cell and will be scaled in each Randle circuit by a factor depending on the local area of the circuit and the global area of the cell.
















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


   
   Get or set the Constant value if positive or load curve ID if negative integer defining the equilibrium voltage (OCV) as a function of the state of charge (SOC).
















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

.. py:property:: temp
   :type: Optional[float]


   
   Get or set the Constant temperature value used for the Randles circuit parameters in case there is no coupling with the thermal solver.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'RANDLES_LAYERED'






