





:class:`LoadThermalVariableNode`
================================


.. py:class:: load_thermal_variable_node.LoadThermalVariableNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_VARIABLE_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalVariableNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~ts`
            - Get or set the Scaled temperature.
          * - :py:attr:`~tb`
            - Get or set the Base temperature.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID that multiplies the scaled temperature, see *DEFINE_ CURVE.


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

    from load_thermal_variable_node import LoadThermalVariableNode

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_VARIABLE_NODE'






