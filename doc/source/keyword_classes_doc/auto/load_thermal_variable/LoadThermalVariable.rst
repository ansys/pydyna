





:class:`LoadThermalVariable`
============================


.. py:class:: load_thermal_variable.LoadThermalVariable(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_VARIABLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalVariable

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Node set ID containing nodes, see *SET_NODE:
          * - :py:attr:`~nsidex`
            - Get or set the Node set ID containing nodes that are exempted (optional), see *SET_ NODE.
          * - :py:attr:`~boxid`
            - Get or set the All nodes in box which belong to NSID are initialized. Others are excluded.
          * - :py:attr:`~ts`
            - Get or set the Scaled temperature.
          * - :py:attr:`~tb`
            - Get or set the Base temperature.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID that multiplies the scaled temperature, see *DEFINE_ CURVE.
          * - :py:attr:`~tse`
            - Get or set the Scaled temperature of the exempted nodes (optional).
          * - :py:attr:`~tbe`
            - Get or set the Base temperature of the exempted nodes (optional).
          * - :py:attr:`~lcide`
            - Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional), see *DEFINE_CURVE.
          * - :py:attr:`~lcidr`
            - Get or set the Load curve ID that multiplies the scaled temperature for dynamic relaxation phase
          * - :py:attr:`~lcidedr`
            - Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional) for dynamic relaxation phase.


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

    from load_thermal_variable import LoadThermalVariable

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID containing nodes, see *SET_NODE:
   EQ.0: all nodes are included.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidex
   :type: int


   
   Get or set the Node set ID containing nodes that are exempted (optional), see *SET_ NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the All nodes in box which belong to NSID are initialized. Others are excluded.
















   ..
       !! processed by numpydoc !!

.. py:property:: ts
   :type: float


   
   Get or set the Scaled temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: tb
   :type: float


   
   Get or set the Base temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID that multiplies the scaled temperature, see *DEFINE_ CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: tse
   :type: float


   
   Get or set the Scaled temperature of the exempted nodes (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: tbe
   :type: float


   
   Get or set the Base temperature of the exempted nodes (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcide
   :type: Optional[int]


   
   Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional), see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidr
   :type: Optional[int]


   
   Get or set the Load curve ID that multiplies the scaled temperature for dynamic relaxation phase
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidedr
   :type: Optional[int]


   
   Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional) for dynamic relaxation phase.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_VARIABLE'






