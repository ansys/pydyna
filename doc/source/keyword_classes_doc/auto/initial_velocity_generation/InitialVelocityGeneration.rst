





:class:`InitialVelocityGeneration`
==================================


.. py:class:: initial_velocity_generation.InitialVelocityGeneration(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_VELOCITY_GENERATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialVelocityGeneration

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Part ID, part set ID, or node set ID.If zero, STYP is ignored,and all velocities are set.
          * - :py:attr:`~styp`
            - Get or set the Set type:
          * - :py:attr:`~omega`
            - Get or set the Angular velocity about rotational axis.
          * - :py:attr:`~vx`
            - Get or set the Initial translational velocity in global x-direction.
          * - :py:attr:`~vy`
            - Get or set the Initial translational velocity in global y-direction.
          * - :py:attr:`~vz`
            - Get or set the Initial translational velocity in global z-direction.
          * - :py:attr:`~ivatn`
            - Get or set the Flag for setting the initial velocities of constrained nodes and parts:
          * - :py:attr:`~icid`
            - Get or set the Local coordinate system ID. The specified translational velocities (VX, VY, VZ) and the direction cosines of the rotation axis (NX, NY, NZ) are in the global system if ICID=0 and are in the local system if ICID is defined. Therefore, if ICID is defined, *INCLUDE_TRANSFORM does not transform (VX, VY, VZ) and (NX, NY, NZ).
          * - :py:attr:`~xc`
            - Get or set the x-coordinate on rotational axis.
          * - :py:attr:`~yc`
            - Get or set the y-coordinate on rotational axis.
          * - :py:attr:`~zc`
            - Get or set the z-coordinate on rotational axis.
          * - :py:attr:`~nx`
            - Get or set the x-direction cosine.  If set to -999, NY and NZ are interpreted as the 1st and 2nd nodes defining the rotational axis, in which case the coordinates of node NY are used as XC, YC, ZC.  If ICID is defined, the direction cosine, (NX, NY, NZ), is projected along coordinate system ICID to yield the direction cosines of the rotation axis only if NX .NE. -999..
          * - :py:attr:`~ny`
            - Get or set the y-direction cosine or the 1st node of the rotational axis when NX = -999.
          * - :py:attr:`~nz`
            - Get or set the z-direction cosine or the 2nd node of the rotational axis when NX = -999..
          * - :py:attr:`~phase`
            - Get or set the Flag determining basis for initialization of velocity.
          * - :py:attr:`~irigid`
            - Get or set the Option to overwrite or automatically set rigid body velocities defined on the *PART_INERTIA and *CONSTRAINED_NODAL_RIGID_BODY _INERTIA cards.


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

    from initial_velocity_generation import InitialVelocityGeneration

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Part ID, part set ID, or node set ID.If zero, STYP is ignored,and all velocities are set.
   WARNING for if IVATN = 0: If a part ID of a rigid body is specified, only the nodes that belong to elements of the rigid body are initialized.Nodes added with* CONSTRAINED_EXTRA_NODES are not initialized.Set IVATN = 1 to initialize velocities of constrained nodes and parts..
















   ..
       !! processed by numpydoc !!

.. py:property:: styp
   :type: int


   
   Get or set the Set type:
   EQ.1: part set ID, see *SET_PART,
   EQ.2: part ID, see *PART,
   EQ.3: nodal set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: omega
   :type: float


   
   Get or set the Angular velocity about rotational axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Initial translational velocity in global x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Initial translational velocity in global y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Initial translational velocity in global z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivatn
   :type: int


   
   Get or set the Flag for setting the initial velocities of constrained nodes and parts:
   EQ.0 : Constrained parts are ignored.
   EQ.1 : Constrained parts and nodes constrained to parts will be assigned initial velocities like the part to which they are constrained..
















   ..
       !! processed by numpydoc !!

.. py:property:: icid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID. The specified translational velocities (VX, VY, VZ) and the direction cosines of the rotation axis (NX, NY, NZ) are in the global system if ICID=0 and are in the local system if ICID is defined. Therefore, if ICID is defined, *INCLUDE_TRANSFORM does not transform (VX, VY, VZ) and (NX, NY, NZ).
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the x-coordinate on rotational axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the y-coordinate on rotational axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the z-coordinate on rotational axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: nx
   :type: float


   
   Get or set the x-direction cosine.  If set to -999, NY and NZ are interpreted as the 1st and 2nd nodes defining the rotational axis, in which case the coordinates of node NY are used as XC, YC, ZC.  If ICID is defined, the direction cosine, (NX, NY, NZ), is projected along coordinate system ICID to yield the direction cosines of the rotation axis only if NX .NE. -999..
















   ..
       !! processed by numpydoc !!

.. py:property:: ny
   :type: float


   
   Get or set the y-direction cosine or the 1st node of the rotational axis when NX = -999.
















   ..
       !! processed by numpydoc !!

.. py:property:: nz
   :type: float


   
   Get or set the z-direction cosine or the 2nd node of the rotational axis when NX = -999..
















   ..
       !! processed by numpydoc !!

.. py:property:: phase
   :type: int


   
   Get or set the Flag determining basis for initialization of velocity.
   EQ.0:   Initial velocities are applied at t = 0 of the regular transient phase of the analysis and are based on the undeformed geometry.Rigid bodies whose velocities are initialized using this keyword should always use PHASE = 0.
   EQ.1 : Initial velocities of deformable bodies are based on geometry that includes deformation incurred prior to the application of the initial velocities.That deformation could be due to a dynamic relaxation phase or due to a nonzero start time specified with *INITIAL_VELOCITY_GENERATION_START_TIME.
















   ..
       !! processed by numpydoc !!

.. py:property:: irigid
   :type: int


   
   Get or set the Option to overwrite or automatically set rigid body velocities defined on the *PART_INERTIA and *CONSTRAINED_NODAL_RIGID_BODY _INERTIA cards.
   EQ.1:  Reset the rigid body velocites for *PART ID or all parts in *SET_PART ID.    This option does not apply for STYP=3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'VELOCITY_GENERATION'






