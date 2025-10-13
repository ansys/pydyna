





:class:`ConstrainedJointStiffnessFlexionTorsion`
================================================


.. py:class:: constrained_joint_stiffness_flexion_torsion.ConstrainedJointStiffnessFlexionTorsion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_STIFFNESS_FLEXION-TORSION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointStiffnessFlexionTorsion

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
          * - :py:attr:`~lcidal`
            - Get or set the Load curve ID for alpha-moment versus rotation in radian, where it should be noted that 0 <= alpha <= pi.
          * - :py:attr:`~lcidg`
            - Get or set the Load curve ID for gamma versus a scale factor which scales the bending moment due to the alpaha rotation. This load curve should be defined in the interval -pi <= gamma <= pi.
          * - :py:attr:`~lcidbt`
            - Get or set the Load curve ID for beta-torsion moment versus twist in radians.
          * - :py:attr:`~dlcidal`
            - Get or set the Load curve ID for alpha-damping moment versus rate of rotation in radians per unit time.
          * - :py:attr:`~dlcidg`
            - Get or set the Load curve ID for gamma-damping scale factor versus rate of rotation in radians per unit time. This scale factor scales the alpha-damping moment.
          * - :py:attr:`~dlcidbt`
            - Get or set the Load curve ID for beta-damping torque versus rate of twist.
          * - :py:attr:`~esal`
            - Get or set the Elastic stiffness per unit radian for friction and stop angles for alpha rotation.
          * - :py:attr:`~fmal`
            - Get or set the Frictional moment limiting value for alpha rotation. If zero, friction is inactive for alpha rotation. This option may also be thought of as an elastic-plastic spring.
          * - :py:attr:`~esbt`
            - Get or set the Elastic stiffness per unit radian for friction and stop angles for beta twist.
          * - :py:attr:`~fmbt`
            - Get or set the Frictional moment limiting value for beta twist. If zero, friction is inactive for beta twist. This option may also be thought of as an elastic-plastic spring.
          * - :py:attr:`~saal`
            - Get or set the Stop angle in degrees for alpha rotation where 0 <= alpha <= pi.
          * - :py:attr:`~nsabt`
            - Get or set the Stop angle in degrees for negative beta rotation.
          * - :py:attr:`~psabt`
            - Get or set the Stop angle in degrees for positive beta rotation.


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

    from constrained_joint_stiffness_flexion_torsion import ConstrainedJointStiffnessFlexionTorsion

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

.. py:property:: lcidal
   :type: int


   
   Get or set the Load curve ID for alpha-moment versus rotation in radian, where it should be noted that 0 <= alpha <= pi.
   If zero, the applied moment is set to zero (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidg
   :type: int


   
   Get or set the Load curve ID for gamma versus a scale factor which scales the bending moment due to the alpaha rotation. This load curve should be defined in the interval -pi <= gamma <= pi.
   If zero, the scale factor defaults to 1. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidbt
   :type: int


   
   Get or set the Load curve ID for beta-torsion moment versus twist in radians.
   If zero, the applied twist is set to zero (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidal
   :type: int


   
   Get or set the Load curve ID for alpha-damping moment versus rate of rotation in radians per unit time.
   If zero, damping is not considered (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidg
   :type: int


   
   Get or set the Load curve ID for gamma-damping scale factor versus rate of rotation in radians per unit time. This scale factor scales the alpha-damping moment.
   If zero, the scale factor defaults to 1. See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidbt
   :type: int


   
   Get or set the Load curve ID for beta-damping torque versus rate of twist.
   If zero, damping is not considered (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: esal
   :type: float


   
   Get or set the Elastic stiffness per unit radian for friction and stop angles for alpha rotation.
   If zero, friction and stop angles are inactive for alpha rotation (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmal
   :type: float


   
   Get or set the Frictional moment limiting value for alpha rotation. If zero, friction is inactive for alpha rotation. This option may also be thought of as an elastic-plastic spring.
















   ..
       !! processed by numpydoc !!

.. py:property:: esbt
   :type: float


   
   Get or set the Elastic stiffness per unit radian for friction and stop angles for beta twist.
   If zero, friction and stop angles are inactive for beta twist (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmbt
   :type: float


   
   Get or set the Frictional moment limiting value for beta twist. If zero, friction is inactive for beta twist. This option may also be thought of as an elastic-plastic spring.
















   ..
       !! processed by numpydoc !!

.. py:property:: saal
   :type: float


   
   Get or set the Stop angle in degrees for alpha rotation where 0 <= alpha <= pi.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: nsabt
   :type: float


   
   Get or set the Stop angle in degrees for negative beta rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: psabt
   :type: float


   
   Get or set the Stop angle in degrees for positive beta rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_STIFFNESS_FLEXION-TORSION'






