





:class:`BoundaryPrescribedOrientationRigidVector`
=================================================


.. py:class:: boundary_prescribed_orientation_rigid_vector.BoundaryPrescribedOrientationRigidVector(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PRESCRIBED_ORIENTATION_RIGID_VECTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPrescribedOrientationRigidVector

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
          * - :py:attr:`~lcidv1`
            - Get or set the Load curve ID specifying the vector measure number vi as a function of time
          * - :py:attr:`~lcidv2`
            - Get or set the Load curve ID specifying the vector measure number vi as a function of time
          * - :py:attr:`~lcidv3`
            - Get or set the Load curve ID specifying the vector measure number vi as a function of time
          * - :py:attr:`~lcids`
            - Get or set the Load curve ID which specifies the spin speed of PIDB about an axis parallel to the vector
          * - :py:attr:`~valspin`
            - Get or set the Value for constant the spin speed of PIDB (radians per unit time).  This option is bypassed if the load curve number defined above is non zero.


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

    from boundary_prescribed_orientation_rigid_vector import BoundaryPrescribedOrientationRigidVector

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

.. py:property:: lcidv1
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the vector measure number vi as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidv2
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the vector measure number vi as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidv3
   :type: Optional[int]


   
   Get or set the Load curve ID specifying the vector measure number vi as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: lcids
   :type: Optional[int]


   
   Get or set the Load curve ID which specifies the spin speed of PIDB about an axis parallel to the vector
















   ..
       !! processed by numpydoc !!

.. py:property:: valspin
   :type: Optional[float]


   
   Get or set the Value for constant the spin speed of PIDB (radians per unit time).  This option is bypassed if the load curve number defined above is non zero.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PRESCRIBED_ORIENTATION_RIGID_VECTOR'






