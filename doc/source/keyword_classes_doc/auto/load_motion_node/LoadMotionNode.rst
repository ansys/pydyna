





:class:`LoadMotionNode`
=======================


.. py:class:: load_motion_node.LoadMotionNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_MOTION_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadMotionNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~node1`
            - Get or set the Node ID for the concentrated force
          * - :py:attr:`~dof1`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE. The  applied force is a function of the applicable degree-of-freedom of NODE2
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.
          * - :py:attr:`~cid1`
            - Get or set the Coordinate system ID (optional), see remark 1 on next page.
          * - :py:attr:`~node2`
            - Get or set the Node ID for calculating the force.
          * - :py:attr:`~dof2`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~cid2`
            - Get or set the Coordinate system ID (optional), see remark 1.


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

    from load_motion_node import LoadMotionNode

Property detail
---------------

.. py:property:: node1
   :type: Optional[int]


   
   Get or set the Node ID for the concentrated force
















   ..
       !! processed by numpydoc !!

.. py:property:: dof1
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ.0: Not valid, please use any of the other available options,
   EQ.1:  x-direction of load action,
   EQ.2:  y-direction of load action,
   EQ.3:  z-direction of load action,
   EQ.4:  moment about the x-axis,
   EQ.5:  moment about the y-axis,
   EQ.6:  moment about the z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE. The  applied force is a function of the applicable degree-of-freedom of NODE2
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid1
   :type: int


   
   Get or set the Coordinate system ID (optional), see remark 1 on next page.
















   ..
       !! processed by numpydoc !!

.. py:property:: node2
   :type: int


   
   Get or set the Node ID for calculating the force.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof2
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ. 1:  x-coordinate
   EQ. 2:  y-coordinate,
   EQ. 3:  z-coordinate,
   EQ. 4:  x-translational displacement,
   EQ. 5:  y-translational displacement,
   EQ. 6:  z-translational displacement,
   EQ. 7:  rotational displacement about the x-axis,
   EQ. 8:  rotational displacement about the y-axis,
   EQ. 9:  rotational displacement about the z-axis.
   EQ.10:  x-translational velocity,
   EQ.11:  y-translational velocity,
   EQ.12:  z-translational velocity,
   EQ.13:  rotational velocity about the x-axis,
   EQ.14:  rotational velocity about the y-axis,
   EQ.15:  rotational velocity about the z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid2
   :type: int


   
   Get or set the Coordinate system ID (optional), see remark 1.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'MOTION_NODE'






