





:class:`ConstrainedRigidBodyStoppers`
=====================================


.. py:class:: constrained_rigid_body_stoppers.ConstrainedRigidBodyStoppers(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_RIGID_BODY_STOPPERS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedRigidBodyStoppers

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of lead rigid body, see *PART.
          * - :py:attr:`~lcmax`
            - Get or set the Load curve ID defining the maximum coordinate or displacement as a function of time. See *DEFINE_CURVE:
          * - :py:attr:`~lcmin`
            - Get or set the Load curve ID defining the minimum coordinate or displacement as a function of time. See *DEFINE_CURVE:
          * - :py:attr:`~psidmx`
            - Get or set the Optional part set ID of rigid bodies that are constrained in the maximum coordinate direction to the lead rigid body.  The part set definition (see *SET_PART_COLUMN) may be used to define the closure distance (D_1 and D_2in Figure 0-1) which activates the constraint.  The constraint does not begin to act until the lead rigid body stops.  If the distance between the lead rigid body is greater than or equal to the closure distance, the constrained rigid body motion away from the lead rigid body also stops.  However, the constrained rigid body is free to move towards the lead rigid body.  If the closure distance is input as zero (0.0), then the constrained rigid body stops when the lead stops.
          * - :py:attr:`~psidmn`
            - Get or set the Optional part set ID of rigid bodies that are constrained in the minimum coordinate direction to the lead rigid body.  The part set definition, (see *SET_PART_COLUMN) may be used to define the closure distance (D_1 and D_2 in Figure 0-1) which activates the constraint.  The constraint does not begin to act until the lead rigid body stops.  If the distance between the lead rigid body is less than or equal to the closure distance, the constrained rigid body motion towards the lead rigid body also stops.  However, the constrained rigid body is free to move away from the lead rigid part.  If the closure distance is input as zero (0.0), then the constrained rigid body stops when the lead stops.
          * - :py:attr:`~lcvmnx`
            - Get or set the Load curve ID which defines the maximum absolute value of the velocity as a function of time that is allowed for the lead rigid body.  See *DEFINE_‌CURVE:
          * - :py:attr:`~dir`
            - Get or set the Direction stopper acts in (reqiured):
          * - :py:attr:`~vid`
            - Get or set the Vector for arbitrary orientation of stopper, see *DEFINE_VECTOR.
          * - :py:attr:`~tb`
            - Get or set the Time at which stopper is activated (default=0.0).
          * - :py:attr:`~td`
            - Get or set the Time at which stopper is deactivated (default = 10^21).
          * - :py:attr:`~stiff`
            - Get or set the Augmentation stiffness for implicit


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

    from constrained_rigid_body_stoppers import ConstrainedRigidBodyStoppers

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of lead rigid body, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmax
   :type: int


   
   Get or set the Load curve ID defining the maximum coordinate or displacement as a function of time. See *DEFINE_CURVE:
   LT.0: Load Curve ID |LCMAX| provides an upper bound for the displacement of the rigid body,
   EQ.0: no limitation of the maximum displacement (default),
   GT.0: Load Curve ID LCMAX provides an upper bound for the position of the rigid body center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmin
   :type: int


   
   Get or set the Load curve ID defining the minimum coordinate or displacement as a function of time. See *DEFINE_CURVE:
   LT.0: Load Curve ID |LCMIN| defines a lower bound for the displacement of the rigid body,
   EQ.0: no limitation of the minimum displacement (default),
   GT.0: Load Curve ID LCMIN defines a lower bound for the position of the rigid body center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: psidmx
   :type: int


   
   Get or set the Optional part set ID of rigid bodies that are constrained in the maximum coordinate direction to the lead rigid body.  The part set definition (see *SET_PART_COLUMN) may be used to define the closure distance (D_1 and D_2in Figure 0-1) which activates the constraint.  The constraint does not begin to act until the lead rigid body stops.  If the distance between the lead rigid body is greater than or equal to the closure distance, the constrained rigid body motion away from the lead rigid body also stops.  However, the constrained rigid body is free to move towards the lead rigid body.  If the closure distance is input as zero (0.0), then the constrained rigid body stops when the lead stops.
















   ..
       !! processed by numpydoc !!

.. py:property:: psidmn
   :type: int


   
   Get or set the Optional part set ID of rigid bodies that are constrained in the minimum coordinate direction to the lead rigid body.  The part set definition, (see *SET_PART_COLUMN) may be used to define the closure distance (D_1 and D_2 in Figure 0-1) which activates the constraint.  The constraint does not begin to act until the lead rigid body stops.  If the distance between the lead rigid body is less than or equal to the closure distance, the constrained rigid body motion towards the lead rigid body also stops.  However, the constrained rigid body is free to move away from the lead rigid part.  If the closure distance is input as zero (0.0), then the constrained rigid body stops when the lead stops.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvmnx
   :type: int


   
   Get or set the Load curve ID which defines the maximum absolute value of the velocity as a function of time that is allowed for the lead rigid body.  See *DEFINE_‌CURVE:
   EQ.0:   no limitation on the velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction stopper acts in (reqiured):
   EQ.1: x-translation,
   EQ.2: y-translation,
   EQ.3: z-translation,
   EQ.4: arbitrary, defined by vector VID (see VID),
   EQ.5: x-axis rotation ,
   EQ.6: y-axis rotation,
   EQ.7: z-axis rotation,
   EQ.8: arbitrary, defined by vector VID (see VID).
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the Vector for arbitrary orientation of stopper, see *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: tb
   :type: float


   
   Get or set the Time at which stopper is activated (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: td
   :type: float


   
   Get or set the Time at which stopper is deactivated (default = 10^21).
















   ..
       !! processed by numpydoc !!

.. py:property:: stiff
   :type: float


   
   Get or set the Augmentation stiffness for implicit
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'RIGID_BODY_STOPPERS'






