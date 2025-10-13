





:class:`LoadNode`
=================


.. py:class:: load_node.LoadNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID, see also *NODE.
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID(see *DEFINE_CURVE) or function ID(see *DEFINE_FUNCTION).
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID (optional).
          * - :py:attr:`~m1`
            - Get or set the Node 1 ID. Only necessary if DOF EQ.4 or EQ.8.
          * - :py:attr:`~m2`
            - Get or set the Node 2 ID. Only necessary if DOF EQ.4 or EQ.8.
          * - :py:attr:`~m3`
            - Get or set the Node 3 ID. Only necessary if DOF EQ.4 or EQ.8.


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

    from load_node import LoadNode

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID, see also *NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ.0: Not valid, please use any of the other available options,
   EQ.1: x-direction of load action (default),
   EQ.2: y-direction of load action,
   EQ.3: z-direction of load action,
   EQ.4: follower force,
   EQ.5: moment about the x-axis,
   EQ.6: moment about the y-axis,
   EQ.7: moment about the z-axis,
   EQ.8: follower moment.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID(see *DEFINE_CURVE) or function ID(see *DEFINE_FUNCTION).
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: m1
   :type: int


   
   Get or set the Node 1 ID. Only necessary if DOF EQ.4 or EQ.8.
















   ..
       !! processed by numpydoc !!

.. py:property:: m2
   :type: int


   
   Get or set the Node 2 ID. Only necessary if DOF EQ.4 or EQ.8.
















   ..
       !! processed by numpydoc !!

.. py:property:: m3
   :type: int


   
   Get or set the Node 3 ID. Only necessary if DOF EQ.4 or EQ.8.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'NODE'






