





:class:`ComponentGebodJointWaist`
=================================


.. py:class:: component_gebod_joint_waist.ComponentGebodJointWaist(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA COMPONENT_GEBOD_JOINT_WAIST keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ComponentGebodJointWaist

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~did`
            - Get or set the Dummy ID, see *COMPONENT_GEBOD_MALE, *COMPONENT_GEBOD_FEMALE, *COMPONENT_GEBOD_CHILD.
          * - :py:attr:`~lc1`
            - Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the first degree of freedom of the joint.
          * - :py:attr:`~lc2`
            - Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the second degree of freedom of the joint.
          * - :py:attr:`~lc3`
            - Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the third degree of freedom of the joint.
          * - :py:attr:`~scf1`
            - Get or set the Scale factor applied to the load curve of the first joint degree of freedom.
          * - :py:attr:`~scf2`
            - Get or set the Scale factor applied to the load curve of the second joint degree of freedom.
          * - :py:attr:`~scf3`
            - Get or set the Scale factor applied to the load curve of the third joint degree of freedom.
          * - :py:attr:`~c1`
            - Get or set the Linear viscous damping coefficient applied to the first DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
          * - :py:attr:`~c2`
            - Get or set the Linear viscous damping coefficient applied to the second DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
          * - :py:attr:`~c3`
            - Get or set the Linear viscous damping coefficient applied to the third DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
          * - :py:attr:`~neut1`
            - Get or set the Neutral angle (degrees) of joint's first DOF.
          * - :py:attr:`~neut2`
            - Get or set the Neutral angle (degrees) of joint's second DOF.
          * - :py:attr:`~neut3`
            - Get or set the Neutral angle (degrees) of joint's third DOF.
          * - :py:attr:`~losa1`
            - Get or set the Value of the low stop angle (degrees) for the first DOF of this joint.
          * - :py:attr:`~hisa1`
            - Get or set the Value of the high stop angle (degrees) for the first DOF of this joint.
          * - :py:attr:`~losa2`
            - Get or set the Value of the low stop angle (degrees) for the second DOF of this joint.
          * - :py:attr:`~hisa2`
            - Get or set the Value of the high stop angle (degrees) for the second DOF of this joint.
          * - :py:attr:`~losa3`
            - Get or set the Value of the low stop angle (degrees) for the third DOF of this joint.
          * - :py:attr:`~hisa3`
            - Get or set the Value of the high stop angle (degrees) for the third DOF of this joint.
          * - :py:attr:`~unk1`
            - Get or set the Unloading stiffness (torque/radian) for the first degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
          * - :py:attr:`~unk2`
            - Get or set the Unloading stiffness (torque/radian) for the second degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
          * - :py:attr:`~unk3`
            - Get or set the Unloading stiffness (torque/radian) for the third degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.


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

    from component_gebod_joint_waist import ComponentGebodJointWaist

Property detail
---------------

.. py:property:: did
   :type: Optional[int]


   
   Get or set the Dummy ID, see *COMPONENT_GEBOD_MALE, *COMPONENT_GEBOD_FEMALE, *COMPONENT_GEBOD_CHILD.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc1
   :type: int


   
   Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the first degree of freedom of the joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc2
   :type: int


   
   Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the second degree of freedom of the joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc3
   :type: int


   
   Get or set the Load curve ID specifying the loading torque versus rotation (in radians) for the third degree of freedom of the joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: scf1
   :type: float


   
   Get or set the Scale factor applied to the load curve of the first joint degree of freedom.
















   ..
       !! processed by numpydoc !!

.. py:property:: scf2
   :type: float


   
   Get or set the Scale factor applied to the load curve of the second joint degree of freedom.
















   ..
       !! processed by numpydoc !!

.. py:property:: scf3
   :type: float


   
   Get or set the Scale factor applied to the load curve of the third joint degree of freedom.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: float


   
   Get or set the Linear viscous damping coefficient applied to the first DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: float


   
   Get or set the Linear viscous damping coefficient applied to the second DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: float


   
   Get or set the Linear viscous damping coefficient applied to the third DOF of the joint. Units are torque*time/radian, where the units of torque and time depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
















   ..
       !! processed by numpydoc !!

.. py:property:: neut1
   :type: float


   
   Get or set the Neutral angle (degrees) of joint's first DOF.
















   ..
       !! processed by numpydoc !!

.. py:property:: neut2
   :type: float


   
   Get or set the Neutral angle (degrees) of joint's second DOF.
















   ..
       !! processed by numpydoc !!

.. py:property:: neut3
   :type: float


   
   Get or set the Neutral angle (degrees) of joint's third DOF.
















   ..
       !! processed by numpydoc !!

.. py:property:: losa1
   :type: float


   
   Get or set the Value of the low stop angle (degrees) for the first DOF of this joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: hisa1
   :type: float


   
   Get or set the Value of the high stop angle (degrees) for the first DOF of this joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: losa2
   :type: float


   
   Get or set the Value of the low stop angle (degrees) for the second DOF of this joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: hisa2
   :type: float


   
   Get or set the Value of the high stop angle (degrees) for the second DOF of this joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: losa3
   :type: float


   
   Get or set the Value of the low stop angle (degrees) for the third DOF of this joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: hisa3
   :type: float


   
   Get or set the Value of the high stop angle (degrees) for the third DOF of this joint.
















   ..
       !! processed by numpydoc !!

.. py:property:: unk1
   :type: float


   
   Get or set the Unloading stiffness (torque/radian) for the first degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
















   ..
       !! processed by numpydoc !!

.. py:property:: unk2
   :type: float


   
   Get or set the Unloading stiffness (torque/radian) for the second degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
















   ..
       !! processed by numpydoc !!

.. py:property:: unk3
   :type: float


   
   Get or set the Unloading stiffness (torque/radian) for the third degree of freedom of the joint. This must be a positive number. Units of torque depend on the choice of UNITS in card 1 of *COMPONENT_GEBOD.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'COMPONENT'


.. py:attribute:: subkeyword
   :value: 'GEBOD_JOINT_WAIST'






