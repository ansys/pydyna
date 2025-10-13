





:class:`ConstrainedJointCoorTranslational`
==========================================


.. py:class:: constrained_joint_coor_translational.ConstrainedJointCoorTranslational(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_COOR_TRANSLATIONAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointCoorTranslational

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rbid_a`
            - Get or set the Part ID of rigid body A
          * - :py:attr:`~rbid_b`
            - Get or set the Part ID of rigid body
          * - :py:attr:`~rps`
            - Get or set the Relative penalty stiffness (default = 1.0).
          * - :py:attr:`~damp`
            - Get or set the Damping scale factor on default damping value. (Revolute and Spherical Joints):
          * - :py:attr:`~tmass`
            - Get or set the Lumped translational mass.  The mass is equally split between the first points defined for rigid bodies A and B.
          * - :py:attr:`~rmass`
            - Get or set the Lumped rotational inertia.  The inertia is equally split between the first points defined for rigid bodies A and B.
          * - :py:attr:`~x1`
            - Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
          * - :py:attr:`~y1`
            - Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
          * - :py:attr:`~z1`
            - Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
          * - :py:attr:`~x2`
            - Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
          * - :py:attr:`~y2`
            - Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
          * - :py:attr:`~z2`
            - Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
          * - :py:attr:`~x3`
            - Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
          * - :py:attr:`~y3`
            - Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
          * - :py:attr:`~z3`
            - Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
          * - :py:attr:`~x4`
            - Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
          * - :py:attr:`~y4`
            - Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
          * - :py:attr:`~z4`
            - Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
          * - :py:attr:`~x5`
            - Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
          * - :py:attr:`~y5`
            - Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
          * - :py:attr:`~z5`
            - Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
          * - :py:attr:`~x6`
            - Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
          * - :py:attr:`~y6`
            - Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
          * - :py:attr:`~z6`
            - Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.


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

    from constrained_joint_coor_translational import ConstrainedJointCoorTranslational

Property detail
---------------

.. py:property:: rbid_a
   :type: Optional[int]


   
   Get or set the Part ID of rigid body A
















   ..
       !! processed by numpydoc !!

.. py:property:: rbid_b
   :type: Optional[int]


   
   Get or set the Part ID of rigid body
















   ..
       !! processed by numpydoc !!

.. py:property:: rps
   :type: float


   
   Get or set the Relative penalty stiffness (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Damping scale factor on default damping value. (Revolute and Spherical Joints):
   EQ.0.0: default is set to 1.0,
   LE.0.01 and GT.0.0: no damping is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmass
   :type: Optional[float]


   
   Get or set the Lumped translational mass.  The mass is equally split between the first points defined for rigid bodies A and B.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmass
   :type: Optional[float]


   
   Get or set the Lumped rotational inertia.  The inertia is equally split between the first points defined for rigid bodies A and B.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: Optional[float]


   
   Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: Optional[float]


   
   Get or set the Coordinate of point 1, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: Optional[float]


   
   Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: Optional[float]


   
   Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: Optional[float]


   
   Get or set the Coordinate of  point 2, in rigid body B.  If points 1 and 2 are coincident in the specified joint type, the coordinate for point 1 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: x3
   :type: Optional[float]


   
   Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: y3
   :type: Optional[float]


   
   Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: z3
   :type: Optional[float]


   
   Get or set the Coordinate of point 3, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: x4
   :type: Optional[float]


   
   Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: y4
   :type: Optional[float]


   
   Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: z4
   :type: Optional[float]


   
   Get or set the Coordinate of  point 4, in rigid body B.  If points 3 and 4 are coincident in the specified joint type, the coordinate for point 3 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: x5
   :type: Optional[float]


   
   Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: y5
   :type: Optional[float]


   
   Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: z5
   :type: Optional[float]


   
   Get or set the Coordinate of point 5, in rigid body A.  Define for all joint types.
















   ..
       !! processed by numpydoc !!

.. py:property:: x6
   :type: Optional[float]


   
   Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: y6
   :type: Optional[float]


   
   Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: z6
   :type: Optional[float]


   
   Get or set the Coordinate of  point 6, in rigid body B.  If points 5 and 6 are coincident in the specified joint type, the coordinate for point 5 is used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_COOR_TRANSLATIONAL'






