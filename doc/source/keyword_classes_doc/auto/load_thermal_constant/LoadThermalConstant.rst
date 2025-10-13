





:class:`LoadThermalConstant`
============================


.. py:class:: load_thermal_constant.LoadThermalConstant(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_CONSTANT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalConstant

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Node set ID containing nodes for initial temperature, see *SET_NODE:
          * - :py:attr:`~nsidex`
            - Get or set the Node set ID containing nodes that are exempted from the imposed temperature (optional).
          * - :py:attr:`~boxid`
            - Get or set the All nodes in box which belong to NSID are initialized. Others are excluded (optional).
          * - :py:attr:`~t`
            - Get or set the Temperature.
          * - :py:attr:`~te`
            - Get or set the Temperature of exempted nodes (optional).


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

    from load_thermal_constant import LoadThermalConstant

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID containing nodes for initial temperature, see *SET_NODE:
   EQ.0: all nodes are included.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidex
   :type: int


   
   Get or set the Node set ID containing nodes that are exempted from the imposed temperature (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the All nodes in box which belong to NSID are initialized. Others are excluded (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: float


   
   Get or set the Temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: te
   :type: float


   
   Get or set the Temperature of exempted nodes (optional).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_CONSTANT'






