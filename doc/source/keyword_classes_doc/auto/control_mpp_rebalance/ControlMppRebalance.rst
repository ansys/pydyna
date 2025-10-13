





:class:`ControlMppRebalance`
============================


.. py:class:: control_mpp_rebalance.ControlMppRebalance(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_MPP_REBALANCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlMppRebalance

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ncycle`
            - Get or set the Number of cycles between rebalance steps
          * - :py:attr:`~icoor`
            - Get or set the Coordinates used in rebalance:
          * - :py:attr:`~icost`
            - Get or set the Element costs used in rebalance:
          * - :py:attr:`~thres`
            - Get or set the Percent threshold for rebalancing when performing in-core adaptivity (see Remark 1). For in-core adaptivity, only include this field


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

    from control_mpp_rebalance import ControlMppRebalance

Property detail
---------------

.. py:property:: ncycle
   :type: Optional[int]


   
   Get or set the Number of cycles between rebalance steps
















   ..
       !! processed by numpydoc !!

.. py:property:: icoor
   :type: int


   
   Get or set the Coordinates used in rebalance:
   EQ.0:   Current coordinates
   NE.0 : Coordinates at t = 0
















   ..
       !! processed by numpydoc !!

.. py:property:: icost
   :type: int


   
   Get or set the Element costs used in rebalance:
   Q.0:    Time costs
   EQ.1 : Original
















   ..
       !! processed by numpydoc !!

.. py:property:: thres
   :type: float


   
   Get or set the Percent threshold for rebalancing when performing in-core adaptivity (see Remark 1). For in-core adaptivity, only include this field
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'MPP_REBALANCE'






