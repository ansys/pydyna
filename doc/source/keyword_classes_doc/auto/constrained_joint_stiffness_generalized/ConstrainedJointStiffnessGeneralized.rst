





:class:`ConstrainedJointStiffnessGeneralized`
=============================================


.. py:class:: constrained_joint_stiffness_generalized.ConstrainedJointStiffnessGeneralized(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_JOINT_STIFFNESS_GENERALIZED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedJointStiffnessGeneralized

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
          * - :py:attr:`~lcidph`
            - Get or set the Load curve ID for x-moment versus rotation in radians.
          * - :py:attr:`~lcidt`
            - Get or set the Load curve ID for y-moment versus rotation in radians.
          * - :py:attr:`~lcidps`
            - Get or set the Load curve ID for z-moment versus rotation in radians.
          * - :py:attr:`~dlcidph`
            - Get or set the Load curve ID for x-damping moment versus rate of rotation in radians per unit time.
          * - :py:attr:`~dlcidt`
            - Get or set the Load curve ID for y-damping moment versus rate of rotation in radians per unit time.
          * - :py:attr:`~dlcidps`
            - Get or set the Load curve ID for z-damping torque versus rate of rotation in radians per unit time.
          * - :py:attr:`~esph`
            - Get or set the Elastic stiffness per unit radian for friction and stop angles for x-rotation.
          * - :py:attr:`~fmph`
            - Get or set the Frictional moment limiting value for x-rotation. If zero, friction is inactive for x-rotation. This option may also be thought of as an elastic-plastic spring.
          * - :py:attr:`~est`
            - Get or set the Elastic stiffness per unit radian for friction and stop angles for y-rotation.
          * - :py:attr:`~fmt`
            - Get or set the Frictional moment limiting value for y-rotation. If zero, friction is inactive for y-rotation. This option may also be thought of as an elastic-plastic spring.
          * - :py:attr:`~esps`
            - Get or set the ESPS Elastic stiffness per unit radian for friction and stop angles for z-rotation.
          * - :py:attr:`~fmps`
            - Get or set the Frictional moment limiting value for z-rotation.
          * - :py:attr:`~nsaph`
            - Get or set the Stop angle in degrees for negative x-rotation.
          * - :py:attr:`~psaph`
            - Get or set the Stop angle in degrees for positive x-rotation.
          * - :py:attr:`~nsat`
            - Get or set the Stop angle in degrees for negative y-rotation.
          * - :py:attr:`~psat`
            - Get or set the Stop angle in degrees for positive y-rotation.
          * - :py:attr:`~nsaps`
            - Get or set the Stop angle in degrees for negative z-rotation.
          * - :py:attr:`~psaps`
            - Get or set the Stop angle in degrees for positive z-rotation.


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

    from constrained_joint_stiffness_generalized import ConstrainedJointStiffnessGeneralized

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

.. py:property:: lcidph
   :type: int


   
   Get or set the Load curve ID for x-moment versus rotation in radians.
   If zero, the applied moment is set to 0.0 (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: int


   
   Get or set the Load curve ID for y-moment versus rotation in radians.
   If zero, the applied moment is set to 0.0 (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidps
   :type: int


   
   Get or set the Load curve ID for z-moment versus rotation in radians.
   If zero, the applied moment is set to 0.0 (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidph
   :type: int


   
   Get or set the Load curve ID for x-damping moment versus rate of rotation in radians per unit time.
   If zero, damping is not considered (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidt
   :type: int


   
   Get or set the Load curve ID for y-damping moment versus rate of rotation in radians per unit time.
   If zero, damping is not considered (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcidps
   :type: int


   
   Get or set the Load curve ID for z-damping torque versus rate of rotation in radians per unit time.
   If zero, damping is not considered (default). See *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: esph
   :type: float


   
   Get or set the Elastic stiffness per unit radian for friction and stop angles for x-rotation.
   If zero, friction and stop angles are inactive for x-rotation (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmph
   :type: float


   
   Get or set the Frictional moment limiting value for x-rotation. If zero, friction is inactive for x-rotation. This option may also be thought of as an elastic-plastic spring.
















   ..
       !! processed by numpydoc !!

.. py:property:: est
   :type: float


   
   Get or set the Elastic stiffness per unit radian for friction and stop angles for y-rotation.
   If zero, friction and stop angles are inactive for y-rotation (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmt
   :type: float


   
   Get or set the Frictional moment limiting value for y-rotation. If zero, friction is inactive for y-rotation. This option may also be thought of as an elastic-plastic spring.
















   ..
       !! processed by numpydoc !!

.. py:property:: esps
   :type: float


   
   Get or set the ESPS Elastic stiffness per unit radian for friction and stop angles for z-rotation.
   If zero, friction and stop angles are inactive for z-rotation (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmps
   :type: float


   
   Get or set the Frictional moment limiting value for z-rotation.
   If zero, friction is inactive for z-rotation (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: nsaph
   :type: float


   
   Get or set the Stop angle in degrees for negative x-rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: psaph
   :type: float


   
   Get or set the Stop angle in degrees for positive x-rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: nsat
   :type: float


   
   Get or set the Stop angle in degrees for negative y-rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: psat
   :type: float


   
   Get or set the Stop angle in degrees for positive y-rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: nsaps
   :type: float


   
   Get or set the Stop angle in degrees for negative z-rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: psaps
   :type: float


   
   Get or set the Stop angle in degrees for positive z-rotation.
   If zero, stop angle is ignored (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'JOINT_STIFFNESS_GENERALIZED'






