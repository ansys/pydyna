





:class:`PartInertiaContactAttachmentNodes`
==========================================


.. py:class:: part_inertia_contact_attachment_nodes.PartInertiaContactAttachmentNodes(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_INERTIA_CONTACT_ATTACHMENT_NODES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartInertiaContactAttachmentNodes

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~title`
            - Get or set the Heading for the part.
          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~secid`
            - Get or set the Section ID defined in *SECTION section.
          * - :py:attr:`~mid`
            - Get or set the Material ID defined in *MAT section.
          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
          * - :py:attr:`~hgid`
            - Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
          * - :py:attr:`~grav`
            - Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
          * - :py:attr:`~adpopt`
            - Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
          * - :py:attr:`~tmid`
            - Get or set the Thermal material property identication defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
          * - :py:attr:`~xc`
            - Get or set the x-coordinate of center of mass. If nodal point, NODEID, is defined XC, YC, and ZC are ignored and the corrdinates of the nodal point, NODID, are taken as the center of mass.
          * - :py:attr:`~yc`
            - Get or set the y-coordinate of center of mass.
          * - :py:attr:`~zc`
            - Get or set the z-coordinate of center of mass.
          * - :py:attr:`~tm`
            - Get or set the Translational mass.
          * - :py:attr:`~ircs`
            - Get or set the Flag for inertia tensor reference coordinate system:
          * - :py:attr:`~nodeid`
            - Get or set the Nodal point defining the CG of the rigid body. This node should be included as an extra node for the rigid body; however, this is not a requirement. If this node is free, its motion will not be updated to correspond with the rigid body after the calculation begins.
          * - :py:attr:`~ixx`
            - Get or set the Ixx, xx component of inertia tensor.
          * - :py:attr:`~ixy`
            - Get or set the Ixy, xy component of inertia tensor.
          * - :py:attr:`~ixz`
            - Get or set the Ixz, xz component of inertia tensor.
          * - :py:attr:`~iyy`
            - Get or set the Iyy, yy component of inertia tensor.
          * - :py:attr:`~iyz`
            - Get or set the Iyz, xy component of inertia tensor.
          * - :py:attr:`~izz`
            - Get or set the Izz , zz component of inertia tensor.
          * - :py:attr:`~vtx`
            - Get or set the Initial translational velocity of rigid body in x-direction.
          * - :py:attr:`~vty`
            - Get or set the Initial translational velocity of rigid body in y-direction.
          * - :py:attr:`~vtz`
            - Get or set the Initial translational velocity of rigid body in z-direction.
          * - :py:attr:`~vrx`
            - Get or set the Initial rotational velocity of rigid body about x-axis.
          * - :py:attr:`~vry`
            - Get or set the Initial rotational velocity of rigid body about y-axis.
          * - :py:attr:`~vrz`
            - Get or set the Initial rotational velocity of rigid body about z-axis.
          * - :py:attr:`~xl`
            - Get or set the x-coordinate of local x-axis. Origin lies at (0,0,0).
          * - :py:attr:`~yl`
            - Get or set the y-coordinate of local x-axis.
          * - :py:attr:`~zl`
            - Get or set the z-coordinate of local x-axis.
          * - :py:attr:`~xlip`
            - Get or set the x-coordinate of vector in local x-y plane.
          * - :py:attr:`~ylip`
            - Get or set the y-coordinate of vector in local x-y plane.
          * - :py:attr:`~zlip`
            - Get or set the z-coordinate of vecotr in local x-y plane.
          * - :py:attr:`~cid`
            - Get or set the Local coordinate system ID, see *DEFINE_COORDINATE_...
          * - :py:attr:`~fs`
            - Get or set the Static coefficient of friction.
          * - :py:attr:`~fd`
            - Get or set the Dynamic coefficient of friction.
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient.
          * - :py:attr:`~vc`
            - Get or set the Viscous friction coefficient.
          * - :py:attr:`~optt`
            - Get or set the Optional contact thickness. For SOFT = 2, it applies to solids, shells and beams. For SOFT = 0 and 1 and for Mortar contacts, it applies to shells and beams only. For SOFT = 0 and 1 with the MPP version, OPTT has a different meaning for solid elements. In this case, OPTT overrides the thickness of solid elements used for the calculation of the contact penetration release (see Table Error! Reference source not found.), but it does not affect the contact thickness
          * - :py:attr:`~sft`
            - Get or set the Optional thickness scale factor for PART ID in automatic contact (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
          * - :py:attr:`~ssf`
            - Get or set the Scale factor on default slave penalty stiffness for this PART ID whenever it appears in the contact definition. If zero, SSF is taken as unity.
          * - :py:attr:`~cparm8`
            - Get or set the Flag to exclude beam-to-beam contact from the same PID for CONTACT_AUTOMATIC_GENERAL.  This applies only to MPP.  Global default may be set using CPARM8 on *CONTACT_..._MPP Optional Card.
          * - :py:attr:`~ansid`
            - Get or set the Attachment node set ID. This option should be used very cautiously and applies only to rigid bodies. The attachment point nodes are updated each cycle whereas other nodes in the rigid body are updated only in the output databases. All loads seen by the rigid body must be applied through this nodal subset or directly to the center of gravity of the rigid body. If the rigid body is in contact this set must include all interacting nodes.


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

    from part_inertia_contact_attachment_nodes import PartInertiaContactAttachmentNodes

Property detail
---------------

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Heading for the part.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID defined in *SECTION section.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID defined in *MAT section.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: int


   
   Get or set the Equation of state ID defined in the *EOS section. Nonzero only for solid elements using an equation of state to compute pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: hgid
   :type: int


   
   Get or set the Hourglass/bulk viscosity ID defined in *HOURGLASS section.
   EQ.0: default values are used.
















   ..
       !! processed by numpydoc !!

.. py:property:: grav
   :type: int


   
   Get or set the Part initialization for gravity loading. This option initializes hydrostatic pressure in the part due to gravity acting on an overburden material. This option applies to brick elements only and must be used with the *LOAD_DENSITY_DEPTH option:
   EQ.0: all parts initialized,
   EQ.1: only current material initialized.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpopt
   :type: Optional[int]


   
   Get or set the Indicate if this part is adapted or not. See also *CONTROL_ADAPTIVITY.
   LT.0: R-adaptive remeshing for 2-D solids, |ADPOPT| gives the load curve ID that defines the element size as a function of time.
   EQ.0:Adaptive remeshing is inactive for this part ID.
   EQ.1:   h - adaptive for 3D shells and for shell / solid / shell sandwich composites.
   EQ.2 : r - adaptive remeshing for 2D solids, 3D tetrahedrons and 3D EFG.For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
   EQ.3 : Axisymmetric r - adaptive remeshing for 3D solid(see Remark 6).For a more detailed description of 3D r - adaptivity, see Volume IV of the Keyword User’s Manual(Multiscale Solvers).
   EQ.9 : Passive h - adaptive for 3D shells.The elements in this part will not be split unless their neighboring elements in other parts need to be split more than one level.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmid
   :type: int


   
   Get or set the Thermal material property identication defined in the *MAT_THERMAL section. Thermal properties must be specified for all solid, shell, and thick shell parts if a thermal or coupled thermal structual/analysis is being performed. Beams and discrete elements are not considered in thermal analyses.
   EQ.0: defaults to MID.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the x-coordinate of center of mass. If nodal point, NODEID, is defined XC, YC, and ZC are ignored and the corrdinates of the nodal point, NODID, are taken as the center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the y-coordinate of center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the z-coordinate of center of mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: tm
   :type: Optional[float]


   
   Get or set the Translational mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: ircs
   :type: int


   
   Get or set the Flag for inertia tensor reference coordinate system:
   EQ.0: global inertia tensor (default),
   EQ.1: principal moments of inertia with orientation vectors.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodeid
   :type: Optional[int]


   
   Get or set the Nodal point defining the CG of the rigid body. This node should be included as an extra node for the rigid body; however, this is not a requirement. If this node is free, its motion will not be updated to correspond with the rigid body after the calculation begins.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixx
   :type: Optional[float]


   
   Get or set the Ixx, xx component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixy
   :type: Optional[float]


   
   Get or set the Ixy, xy component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: ixz
   :type: Optional[float]


   
   Get or set the Ixz, xz component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyy
   :type: Optional[float]


   
   Get or set the Iyy, yy component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: iyz
   :type: Optional[float]


   
   Get or set the Iyz, xy component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: izz
   :type: Optional[float]


   
   Get or set the Izz , zz component of inertia tensor.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtx
   :type: Optional[float]


   
   Get or set the Initial translational velocity of rigid body in x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vty
   :type: Optional[float]


   
   Get or set the Initial translational velocity of rigid body in y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtz
   :type: Optional[float]


   
   Get or set the Initial translational velocity of rigid body in z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vrx
   :type: Optional[float]


   
   Get or set the Initial rotational velocity of rigid body about x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vry
   :type: Optional[float]


   
   Get or set the Initial rotational velocity of rigid body about y-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: vrz
   :type: Optional[float]


   
   Get or set the Initial rotational velocity of rigid body about z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xl
   :type: Optional[float]


   
   Get or set the x-coordinate of local x-axis. Origin lies at (0,0,0).
















   ..
       !! processed by numpydoc !!

.. py:property:: yl
   :type: Optional[float]


   
   Get or set the y-coordinate of local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: zl
   :type: Optional[float]


   
   Get or set the z-coordinate of local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: xlip
   :type: Optional[float]


   
   Get or set the x-coordinate of vector in local x-y plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: ylip
   :type: Optional[float]


   
   Get or set the y-coordinate of vector in local x-y plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: zlip
   :type: Optional[float]


   
   Get or set the z-coordinate of vecotr in local x-y plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID, see *DEFINE_COORDINATE_...
   If defined, leave fields 1-6 blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: Optional[float]


   
   Get or set the Static coefficient of friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: Optional[float]


   
   Get or set the Dynamic coefficient of friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: Optional[float]


   
   Get or set the Exponential decay coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: vc
   :type: Optional[float]


   
   Get or set the Viscous friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: optt
   :type: Optional[float]


   
   Get or set the Optional contact thickness. For SOFT = 2, it applies to solids, shells and beams. For SOFT = 0 and 1 and for Mortar contacts, it applies to shells and beams only. For SOFT = 0 and 1 with the MPP version, OPTT has a different meaning for solid elements. In this case, OPTT overrides the thickness of solid elements used for the calculation of the contact penetration release (see Table Error! Reference source not found.), but it does not affect the contact thickness
















   ..
       !! processed by numpydoc !!

.. py:property:: sft
   :type: Optional[float]


   
   Get or set the Optional thickness scale factor for PART ID in automatic contact (scales true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssf
   :type: Optional[float]


   
   Get or set the Scale factor on default slave penalty stiffness for this PART ID whenever it appears in the contact definition. If zero, SSF is taken as unity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cparm8
   :type: Optional[float]


   
   Get or set the Flag to exclude beam-to-beam contact from the same PID for CONTACT_AUTOMATIC_GENERAL.  This applies only to MPP.  Global default may be set using CPARM8 on *CONTACT_..._MPP Optional Card.
   EQ.0 : Flag is not set(default).
   EQ.1 : Flag is set.
   EQ.2 : Flag is set.CPARM8 = 2 has the additional effect of permitting contact treatment of spot weld(type 9) beams in AUTOMATIC_GENERAL contacts; spot weld beams are otherwise disregarded entirely by AUTOMATIC_GENERAL contacts.
















   ..
       !! processed by numpydoc !!

.. py:property:: ansid
   :type: int


   
   Get or set the Attachment node set ID. This option should be used very cautiously and applies only to rigid bodies. The attachment point nodes are updated each cycle whereas other nodes in the rigid body are updated only in the output databases. All loads seen by the rigid body must be applied through this nodal subset or directly to the center of gravity of the rigid body. If the rigid body is in contact this set must include all interacting nodes.
   EQ.0: All nodal updates are skipped for this rigid body. The null option can be used if the rigid body is fixed in space or if the rigid body does not interact with other parts, e.g., the rigid body is only used for some visual purpose (default).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'INERTIA_CONTACT_ATTACHMENT_NODES'






