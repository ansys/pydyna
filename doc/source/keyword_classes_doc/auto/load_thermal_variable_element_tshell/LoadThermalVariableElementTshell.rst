





:class:`LoadThermalVariableElementTshell`
=========================================


.. py:class:: load_thermal_variable_element_tshell.LoadThermalVariableElementTshell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_VARIABLE_ELEMENT_TSHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalVariableElementTshell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Element ID.
          * - :py:attr:`~ts`
            - Get or set the Scaled temperature
          * - :py:attr:`~tb`
            - Get or set the Base temperature
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining a scale factor that multiplies the scaled temperature     as a function of time, (see *DEFINE_CURVE).


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

    from load_thermal_variable_element_tshell import LoadThermalVariableElementTshell

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ts
   :type: Optional[float]


   
   Get or set the Scaled temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: tb
   :type: Optional[float]


   
   Get or set the Base temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining a scale factor that multiplies the scaled temperature     as a function of time, (see *DEFINE_CURVE).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_VARIABLE_ELEMENT_TSHELL'






