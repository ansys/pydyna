





:class:`AleSmoothing`
=====================


.. py:class:: ale_smoothing.AleSmoothing(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_SMOOTHING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleSmoothing

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dnid`
            - Get or set the Dependent node or node set ID:
          * - :py:attr:`~nid1`
            - Get or set the ID of first node or set for constraining the dependent nodes:
          * - :py:attr:`~nid2`
            - Get or set the ID of second node or node set for constraining the dependent nodes:
          * - :py:attr:`~ipre`
            - Get or set the EQ.0: smoothing constraints are performed after mesh relaxation,
          * - :py:attr:`~xco`
            - Get or set the x-coordinate of constraint vector.
          * - :py:attr:`~yco`
            - Get or set the y-coordinate of constraint vector.
          * - :py:attr:`~zco`
            - Get or set the z-coordinate of constraint vector.


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

    from ale_smoothing import AleSmoothing

Property detail
---------------

.. py:property:: dnid
   :type: Optional[int]


   
   Get or set the Dependent node or node set ID:
   GT.0: DNID is an ALE node.
   EQ.0 : The dependent nodes are the nodes of an ALE mesh connected to the nodes in INID1.See Remark 2
   LT.0: -DNID is the ID of an ALE node set.See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: nid1
   :type: Optional[int]


   
   Get or set the ID of first node or set for constraining the dependent nodes:
   GT.0: NID1 is a node.
   LT.0 : -NID1 is a segment set ID if XCO = YCO = ZCO = 0.0.
   Otherwise, -NID1 is a node set ID.See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: Optional[int]


   
   Get or set the ID of second node or node set for constraining the dependent nodes:
   GT.0 : NID2 is a node.
   EQ.0 : The dependent node motion is solely controlled by
   NID1.See Remarks 2 and 3.
   LT.0 : -NID2 is a node set ID.See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipre
   :type: int


   
   Get or set the EQ.0: smoothing constraints are performed after mesh relaxation,
   EQ.1: Smoothing constraints are performed before mesh relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: xco
   :type: float


   
   Get or set the x-coordinate of constraint vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: yco
   :type: float


   
   Get or set the y-coordinate of constraint vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: zco
   :type: float


   
   Get or set the z-coordinate of constraint vector.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'SMOOTHING'






