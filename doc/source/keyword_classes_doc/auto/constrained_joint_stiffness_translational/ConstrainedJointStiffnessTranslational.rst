





:class:`ConstrainedJointStiffnessTranslational`
===============================================


.. py:class:: constrained_joint_stiffness_translational.ConstrainedJointStiffnessTranslational(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_STIFFNESS_TRANSLATIONAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointStiffnessTranslational

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~jsid`
            - Get or set the Joint stiffness ID.
          * - :py:attr:`~pida`
            - Get or set the Part ID for rigid body A, see *PART.
          * - :py:attr:`~pidb`
            - Get or set the Part ID for rigid body B, see *PART.
          * - :py:attr:`~cida`
            - Get or set the Coordinate ID for rigid body A, see *DEFINE_COORDINATE_OPTION.
          * - :py:attr:`~cidb`
            - Get or set the Coordinate ID for rigid body B.
          * - :py:attr:`~jid`
            - Get or set the Joint ID for the joint reaction forces. If zero, tables can t be used in place of load curves for defining the frictional moments.
          * - :py:attr:`~lcidx`
            - Get or set the Load curve ID for x force versus x-translational relative displacement between the origins of CIDA and CIDB based on the x-direction of CIDB. If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
          * - :py:attr:`~lcidy`
            - Get or set the Load curve ID for y force versus y-translational relative displacement between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
          * - :py:attr:`~lcidz`
            - Get or set the Load curve ID for z force versus z-translational relative displacement between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
          * - :py:attr:`~dlcidx`
            - Get or set the Load curve ID for x damping force versus rate of x-translational displacement per unit time between the origins of CIDA and CIDB based on the x-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
          * - :py:attr:`~dlcidy`
            - Get or set the Load curve ID for y damping force versus rate of y-translational displacement per unit time between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
          * - :py:attr:`~dlcidz`
            - Get or set the Load curve ID for z damping force versus rate of z-translational displacement per unit time between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
          * - :py:attr:`~esx`
            - Get or set the Elastic stiffness for friction and stop displacement for x-translation.  If zero, friction and stop angles are inactive for x-translation.
          * - :py:attr:`~ffx`
            - Get or set the Frictional force limiting value for x-translation.  If zero, friction is inactive for x-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus x-translation.
          * - :py:attr:`~esy`
            - Get or set the Elastic stiffness for friction and stop displacement for y-translation.   If zero, friction and stop angles are inactive for y-translation.
          * - :py:attr:`~ffy`
            - Get or set the Frictional force limiting value for y-translation.  If zero, friction is inactive for y-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus y-translation.
          * - :py:attr:`~esz`
            - Get or set the Elastic stiffness for friction and stop displacement for z-translation.  If zero, friction and stop angles are inactive for z-translation.
          * - :py:attr:`~ffz`
            - Get or set the Frictional force limiting value for z-translation.  If zero, friction is inactive for z-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus z-translation.
          * - :py:attr:`~nsdx`
            - Get or set the Stop displacement for negative x-translation.  Ignored if zero.
          * - :py:attr:`~psdx`
            - Get or set the Stop displacement for positive x-translation.  Ignored if zero.
          * - :py:attr:`~nsdy`
            - Get or set the Stop displacement for negative y-translation.  Ignored if zero.
          * - :py:attr:`~psdy`
            - Get or set the Stop displacement for positive y-translation.  Ignored if zero.
          * - :py:attr:`~nsdz`
            - Get or set the Stop displacement for negative z-translation.  Ignored if zero.
          * - :py:attr:`~psdz`
            - Get or set the Stop displacement for positive z-translation.  Ignored if zero.


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

    from constrained_joint_stiffness_translational import ConstrainedJointStiffnessTranslational

Property detail
---------------

.. py:property:: jsid
   :type: Optional[int]


   
   Get or set the Joint stiffness ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pida
   :type: Optional[int]


   
   Get or set the Part ID for rigid body A, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidb
   :type: Optional[int]


   
   Get or set the Part ID for rigid body B, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: cida
   :type: Optional[int]


   
   Get or set the Coordinate ID for rigid body A, see *DEFINE_COORDINATE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: cidb
   :type: int


   
   Get or set the Coordinate ID for rigid body B.
   If zero, the coordinate ID for rigid body A is used (default).See *DEFINE_COORDINATE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: jid
   :type: Optional[int]


   
   Get or set the Joint ID for the joint reaction forces. If zero, tables can t be used in place of load curves for defining the frictional moments.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidx
   :type: Optional[int]


   
   Get or set the Load curve ID for x force versus x-translational relative displacement between the origins of CIDA and CIDB based on the x-direction of CIDB. If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: Optional[int]


   
   Get or set the Load curve ID for y force versus y-translational relative displacement between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: Optional[int]


   
   Get or set the Load curve ID for z force versus z-translational relative displacement between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, the applied force is set to 0.0. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidx
   :type: Optional[int]


   
   Get or set the Load curve ID for x damping force versus rate of x-translational displacement per unit time between the origins of CIDA and CIDB based on the x-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidy
   :type: Optional[int]


   
   Get or set the Load curve ID for y damping force versus rate of y-translational displacement per unit time between the origins of CIDA and CIDB based on the y-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidz
   :type: Optional[int]


   
   Get or set the Load curve ID for z damping force versus rate of z-translational displacement per unit time between the origins of CIDA and CIDB based on the z-direction of CIDB.  If zero, damping is not considered.  See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: esx
   :type: float


   
   Get or set the Elastic stiffness for friction and stop displacement for x-translation.  If zero, friction and stop angles are inactive for x-translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ffx
   :type: float


   
   Get or set the Frictional force limiting value for x-translation.  If zero, friction is inactive for x-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus x-translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: esy
   :type: float


   
   Get or set the Elastic stiffness for friction and stop displacement for y-translation.   If zero, friction and stop angles are inactive for y-translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ffy
   :type: float


   
   Get or set the Frictional force limiting value for y-translation.  If zero, friction is inactive for y-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus y-translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: esz
   :type: float


   
   Get or set the Elastic stiffness for friction and stop displacement for z-translation.  If zero, friction and stop angles are inactive for z-translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ffz
   :type: float


   
   Get or set the Frictional force limiting value for z-translation.  If zero, friction is inactive for z-translation.  This option may also be thought of as an elastic-plastic spring.  If a negative value is input then the absolute value is taken as the load curve ID defining the yield force versus z-translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsdx
   :type: Optional[float]


   
   Get or set the Stop displacement for negative x-translation.  Ignored if zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: psdx
   :type: Optional[float]


   
   Get or set the Stop displacement for positive x-translation.  Ignored if zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsdy
   :type: Optional[float]


   
   Get or set the Stop displacement for negative y-translation.  Ignored if zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: psdy
   :type: Optional[float]


   
   Get or set the Stop displacement for positive y-translation.  Ignored if zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsdz
   :type: Optional[float]


   
   Get or set the Stop displacement for negative z-translation.  Ignored if zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: psdz
   :type: Optional[float]


   
   Get or set the Stop displacement for positive z-translation.  Ignored if zero.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_STIFFNESS_TRANSLATIONAL'






