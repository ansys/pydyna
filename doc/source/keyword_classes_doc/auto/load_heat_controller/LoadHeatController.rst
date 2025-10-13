





:class:`LoadHeatController`
===========================


.. py:class:: load_heat_controller.LoadHeatController(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_HEAT_CONTROLLER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadHeatController

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~node`
            - Get or set the Sensor is located at this node number.
          * - :py:attr:`~pid`
            - Get or set the Part ID assigned to the elements modeling the heater or cooler being controlled
          * - :py:attr:`~load`
            - Get or set the Heater output (q0) [typical units W/m3]
          * - :py:attr:`~tset`
            - Get or set the Controller set point temperature at location identified by NODE
          * - :py:attr:`~type`
            - Get or set the Type of control function:
          * - :py:attr:`~gp`
            - Get or set the Proportional gain
          * - :py:attr:`~gi`
            - Get or set the Integral gain


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

    from load_heat_controller import LoadHeatController

Property detail
---------------

.. py:property:: node
   :type: Optional[int]


   
   Get or set the Sensor is located at this node number.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID assigned to the elements modeling the heater or cooler being controlled
















   ..
       !! processed by numpydoc !!

.. py:property:: load
   :type: Optional[float]


   
   Get or set the Heater output (q0) [typical units W/m3]
















   ..
       !! processed by numpydoc !!

.. py:property:: tset
   :type: Optional[float]


   
   Get or set the Controller set point temperature at location identified by NODE
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: Optional[int]


   
   Get or set the Type of control function:
   EQ.1: on-off
   EQ.2: proportional + integral
















   ..
       !! processed by numpydoc !!

.. py:property:: gp
   :type: Optional[float]


   
   Get or set the Proportional gain
















   ..
       !! processed by numpydoc !!

.. py:property:: gi
   :type: Optional[float]


   
   Get or set the Integral gain
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'HEAT_CONTROLLER'






