





:class:`BoundaryPrescribedOrientationRigidAngles`
=================================================


.. py:class:: boundary_prescribed_orientation_rigid_angles.BoundaryPrescribedOrientationRigidAngles(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PRESCRIBED_ORIENTATION_RIGID_ANGLES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPrescribedOrientationRigidAngles

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pidb`
            - Get or set the Part ID for rigid body B whose orientation is prescribed
          * - :py:attr:`~pida`
            - Get or set the Part ID for rigid body A.  The orientation of PIDB is measured with respect to the coordinate system of PIDA, as defined by LCO on *MAT_RIGID.  If zero then orientation of PIDB is measured with respect to the global reference frame except for BODY=1 in the ANGLES option
          * - :py:attr:`~intrp`
            - Get or set the Interpolation method used on time history curves:
          * - :py:attr:`~birth`
            - Get or set the Prior to this time the body moves freely under the action of other agents.
          * - :py:attr:`~death`
            - Get or set the The body is freed at this time and subsequently allowed to move under the action of other agents
          * - :py:attr:`~toffset`
            - Get or set the Time offset flag:
          * - :py:attr:`~lcidq1`
            - Get or set the Load curve ID.
          * - :py:attr:`~lcidq2`
            - Get or set the Load curve ID.
          * - :py:attr:`~lcidq3`
            - Get or set the Load curve ID.
          * - :py:attr:`~iseq`
            - Get or set the Specifies the sequence in which the rotations are effected.  In this first set of sequences three unique axes are involved.
          * - :py:attr:`~ishft`
            - Get or set the Angle shift.
          * - :py:attr:`~body`
            - Get or set the Reference axes.


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

    from boundary_prescribed_orientation_rigid_angles import BoundaryPrescribedOrientationRigidAngles

Property detail
---------------

.. py:property:: pidb
   :type: Optional[int]


   
   Get or set the Part ID for rigid body B whose orientation is prescribed
















   ..
       !! processed by numpydoc !!

.. py:property:: pida
   :type: Optional[int]


   
   Get or set the Part ID for rigid body A.  The orientation of PIDB is measured with respect to the coordinate system of PIDA, as defined by LCO on *MAT_RIGID.  If zero then orientation of PIDB is measured with respect to the global reference frame except for BODY=1 in the ANGLES option
















   ..
       !! processed by numpydoc !!

.. py:property:: intrp
   :type: int


   
   Get or set the Interpolation method used on time history curves:
   EQ.1: Linear interpolation (default)
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Prior to this time the body moves freely under the action of other agents.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the The body is freed at this time and subsequently allowed to move under the action of other agents
















   ..
       !! processed by numpydoc !!

.. py:property:: toffset
   :type: int


   
   Get or set the Time offset flag:
   EQ.0:   No time offset is applied.
   EQ.1:   The time value of all load curves will be offset by the birth time,
   EQ.0:   no time offset is applied
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidq1
   :type: Optional[int]


   
   Get or set the Load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidq2
   :type: Optional[int]


   
   Get or set the Load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidq3
   :type: Optional[int]


   
   Get or set the Load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: iseq
   :type: int


   
   Get or set the Specifies the sequence in which the rotations are effected.  In this first set of sequences three unique axes are involved.
   EQ.123:  the first rotation is performed about the x axis an amount q1, the second about the y axis an amount q2 and the third about the z axis an amount q3.
   EQ.231:  the first rotation is performed about the y axis an amount q1, the second about the z axis an amount q2 and the third about the x axis an amount q3.
   EQ.312:  the first rotation is performed about the z axis an amount q1, the second about the x axis an amount q2 and the third about the y axis an amount q3.
   EQ.132:  the first rotation is performed about the x axis an amount q1, the second about the z axis an amount q2 and the third about the y axis an amount q3.
   EQ.213:  the first rotation is performed about the y axis an amount q1, the second about the x axis an amount q2 and the third about the z axis an amount q3.
   EQ.321:  the first rotation is performed about the z axis an amount q1, the second about the y axis an amount q2 and the third about the x axis an amount q3.
   The second set of sequences involve only two unique axes where the first and third are repeated.
   EQ.121:  the first rotation is performed about the x axis an amount q1, the second about the y axis an amount q2 and the third about the x axis an amount q3.
   EQ.131:  the first rotation is performed about the x axis an amount q1, the second about the z axis an amount q2 and the third about the x axis an amount q3.
   VARIABLE DESCRIPTION
   EQ.212:  the first rotation is performed about the y axis an amount q1, the second about the x axis an amount q2 and the third about the y axis an amount q3.
   EQ.232:  the first rotation is performed about the y axis an amount q1, the second about the z axis an amount q2 and the third about the y axis an amount q3.
   EQ.313:  the first rotation is performed about the z axis an amount q1, the second about the x axis an amount q2 and the third about the z axis an amount q3.
   EQ.323:  the first rotation is performed about the z axis an amount q1, the second about the x axis an amount q2 and the third about the z axis an amount q3..
















   ..
       !! processed by numpydoc !!

.. py:property:: ishft
   :type: int


   
   Get or set the Angle shift.
   EQ.1:  Angle curves are unaltered.
   EQ.2: Shifts angle data in the LCIDQi curves as necessary to eliminate discontinuities. If angles are confined to the range [- , ] and the data contains excursions exceeding   then set ISHFT=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: body
   :type: int


   
   Get or set the Reference axes.
   EQ.0: Rotations are performed about axes fixed in PIDA (extrinsic rotation, default).
   EQ.1: Rotations are performed about axes fixed in PIDB (intrinsic rotation).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PRESCRIBED_ORIENTATION_RIGID_ANGLES'






