





:class:`BoundaryPrescribedMotionSetBox`
=======================================


.. py:class:: boundary_prescribed_motion_set_box.BoundaryPrescribedMotionSetBox(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PRESCRIBED_MOTION_SET_BOX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPrescribedMotionSetBox

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~typeid`
            - Get or set the nodal set ID (SID in *SET_NODE)
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~vad`
            - Get or set the Velocity/Acceleration/Displacement flag:
          * - :py:attr:`~lcid`
            - Get or set the Curve ID or function ID to describe motion value as a function of time; see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If LCID refers to *DEFINE_FUNCTION, the function has four arguments: time and x, y and z coordinates of the node or rigid body, such as f(t,x,y,z)=10.0×t+max⁡(x-100,0.). If VAD = 2, the function has one argument which is time, such as f(t)=10.0×t (see Remark 2). See BIRTH below.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor (default=1.0).
          * - :py:attr:`~vid`
            - Get or set the Vector ID for DOF values of 4 or 8, see *DEFINE_VECTOR.
          * - :py:attr:`~death`
            - Get or set the Time imposed motion/constraint is removed (default=1.0E+28).
          * - :py:attr:`~birth`
            - Get or set the Time imposed motion/constraint is activated (default=0.0).
          * - :py:attr:`~boxid`
            - Get or set the A box ID defining a box region in space in which the constraint is activated. Only the nodes falling inside the box will be applied the  prescribed motion.
          * - :py:attr:`~toffset`
            - Get or set the Time offset flag for the SET_BOX option:
          * - :py:attr:`~lcbchk`
            - Get or set the Optional load curve allowing more flexible and efficient use of SET_BOX option. Instead of performing box-check at every time
          * - :py:attr:`~offset1`
            - Get or set the Offset for DOF types 9-11 (y, z, x direction).
          * - :py:attr:`~offset2`
            - Get or set the Offset for DOF types 9-11 (z, x, y direction.)
          * - :py:attr:`~lrb`
            - Get or set the lead rigid body for measuring the relative displacement.
          * - :py:attr:`~node1`
            - Get or set the Optional orientation node, n1, for relative displacement.
          * - :py:attr:`~node2`
            - Get or set the Optional orientation node, n2, for relative displacement.


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

    from boundary_prescribed_motion_set_box import BoundaryPrescribedMotionSetBox

Property detail
---------------

.. py:property:: typeid
   :type: Optional[int]


   
   Get or set the nodal set ID (SID in *SET_NODE)
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ.0: Not valid, please use any of the other available options,
   EQ.1: x-translational DOF,
   EQ.2: y-translational DOF,
   EQ.3: z-translational DOF,
   EQ.4: translational motion only in direction given by the VID. Movement on plane normal to the vector is permitted,
   EQ.-4: Same as 4, except translation on the plane normal to the vector is NOT permitted,
   EQ.5: x-rotational DOF,
   EQ.6: y-rotational DOF,
   EQ.7: z-rotational DOF,
   EQ.8: rotational motion about an axis which is passing through the center-of-gravity of the node, node set, or rigid body and is parallel to vector VID.  Rotation about the normal axes is permitted,
   EQ.-8:rotational motion about an axis which is passing through the center-of-gravity of the node or node set and is parallel to vector VID.  Rotation about the normal axes is not permitted.  This option does not apply to rigid bodies.,
   EQ.9: y/z DOF for node rotating about the x-axis at location (OFFSET1,OFFSET2) in the yz-plane, point (y,z). Radial motion is NOT permitted,
   EQ.-9: Same as 9, except radial motion is permitted,
   EQ.10: z/x DOF for node rotating about the y-axis at location (OFFSET1,OFFSET2) in the zx-plane, point(z,x). Radial motion is NOT permitted,
   EQ.-10:Same as  10, except radial motion is permitted,
   EQ.11: x/y DOF for node rotating about the z-axis at location (OFFSET1,OFFSET2) in the xy-plane, point (x,y). Radial motion is NOT permitted,
   EQ.-11: Same as 11, except radial motion is permitted.
   EQ.12:  Translational motion in direction given by the normals to the segments.  Applicable to SET_‌SEGMENT option only
















   ..
       !! processed by numpydoc !!

.. py:property:: vad
   :type: int


   
   Get or set the Velocity/Acceleration/Displacement flag:
   EQ.0: velocity(rigid bodies and nodes),
   EQ.1: acceleration(nodes only),
   EQ.2: displacement(rigid bodies and nodes).
   EQ.3: velocity versus displacement(rigid bodies),
   EQ.4: relative displacement(rigid bodies only)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Curve ID or function ID to describe motion value as a function of time; see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If LCID refers to *DEFINE_FUNCTION, the function has four arguments: time and x, y and z coordinates of the node or rigid body, such as f(t,x,y,z)=10.0×t+max⁡(x-100,0.). If VAD = 2, the function has one argument which is time, such as f(t)=10.0×t (see Remark 2). See BIRTH below.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID for DOF values of 4 or 8, see *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time imposed motion/constraint is removed (default=1.0E+28).
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time imposed motion/constraint is activated (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: Optional[int]


   
   Get or set the A box ID defining a box region in space in which the constraint is activated. Only the nodes falling inside the box will be applied the  prescribed motion.
















   ..
       !! processed by numpydoc !!

.. py:property:: toffset
   :type: int


   
   Get or set the Time offset flag for the SET_BOX option:
   EQ.1: the time value of the load curve, LCID, will be offset by the
   time when the node enters the box,
   EQ.0: no time offset is applied to LCID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcbchk
   :type: int


   
   Get or set the Optional load curve allowing more flexible and efficient use of SET_BOX option. Instead of performing box-check at every time
   step, discrete box-check times could be given as x-values of LCBCHK. LCBCHKs y-values specify corresponding death times.
   For example, a curve with points (20, 30) and (50, 70) will result in two box checks. The first will occur at 20, and the prescribed motion
   will be active from 20 to 30. The second will occur at 50, and the prescribed motion will be active from 50 to 70. A y-value of  0
   means the prescribed motion will stay active until next box-check. For example, an additional 3 rd point of (90, 0) will lead to another
   box-check at 90, and the prescribed motion will be active from 90 until the end of the simulation..
















   ..
       !! processed by numpydoc !!

.. py:property:: offset1
   :type: float


   
   Get or set the Offset for DOF types 9-11 (y, z, x direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: offset2
   :type: float


   
   Get or set the Offset for DOF types 9-11 (z, x, y direction.)
















   ..
       !! processed by numpydoc !!

.. py:property:: lrb
   :type: int


   
   Get or set the lead rigid body for measuring the relative displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: node1
   :type: int


   
   Get or set the Optional orientation node, n1, for relative displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: node2
   :type: int


   
   Get or set the Optional orientation node, n2, for relative displacement.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PRESCRIBED_MOTION_SET_BOX'






