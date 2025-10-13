





:class:`ContactGebodRightFoot`
==============================


.. py:class:: contact_gebod_right_foot.ContactGebodRightFoot(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_GEBOD_RIGHT_FOOT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactGebodRightFoot

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~did`
            - Get or set the Dummy ID, see *COMPONENT_GEBOD.
          * - :py:attr:`~ssid`
            - Get or set the Slave set ID, see *SET_NODE_OPTION, *PART, or *SET_PART.
          * - :py:attr:`~sstyp`
            - Get or set the Slave set type:
          * - :py:attr:`~sf`
            - Get or set the Penalty scale factor. Useful to scale maximized penalty (default=1.0).
          * - :py:attr:`~df`
            - Get or set the Damping option, see description for *CONTACT_OPTION:
          * - :py:attr:`~cf`
            - Get or set the Coulomb friction coefficient. Assumed to be constant (default=0.5).
          * - :py:attr:`~intord`
            - Get or set the Integration order (slaved materials only).
          * - :py:attr:`~bt`
            - Get or set the Birth time (default=0.0).
          * - :py:attr:`~dt`
            - Get or set the Death time (default=1.0E+20).
          * - :py:attr:`~so`
            - Get or set the Flag to use penalty stiffness as in surface to surface contact:
          * - :py:attr:`~cid`
            - Get or set the ID keyword option
          * - :py:attr:`~heading`
            - Get or set the Interface descriptor. We suggest using unique descriptions.
          * - :py:attr:`~ignore`
            - Get or set the By setting this variable to 1, the "ignore initial penetrations" option is turned on for this contact.  Alternatively, this option may be turned on by setting IGNORE = 1 on Card 4 of *CONTROL_CONTACT or on Optional Card C of *CONTACT.  In other words, if IGNORE is set to 1 in any of three places, initial penetrations are tracked.
          * - :py:attr:`~bckt`
            - Get or set the Bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts. For these two exceptions, the BSORT option on Optional Card A applies instead.
          * - :py:attr:`~lcbckt`
            - Get or set the Load curve for bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts.  For the two exceptions, the negative BSORT option on Optional Card A applies instead.
          * - :py:attr:`~ns2trk`
            - Get or set the Number of potential contacts to track for each tracked node.  The normal input for this (DEPTH on Optional Card A) is ignored..
          * - :py:attr:`~inititr`
            - Get or set the Number of iterations to perform when trying to eliminate initial penetrations.  Note that an input of 0 means 0, not the default value (which is 2).  Leaving this field blank will set INITITR to 2.
          * - :py:attr:`~parmax`
            - Get or set the The parametric extension distance for contact segments.  The MAXPAR parameter on Optional Card A is not used for MPP.  For non-tied contacts, the default is 1.0005. For tied contacts the default is 1.035 and, the actual extension used is computed as follows: see the manual
          * - :py:attr:`~cparm8`
            - Get or set the Flag for behavior of AUTOMATIC_GENERAL contacts.  CPARM8's value is interpreted as two separate flags: OPT1 and OPT2 according to the rule,
          * - :py:attr:`~mpp2`
            - Get or set the Flag whether this is the MPP card.
          * - :py:attr:`~chksegs`
            - Get or set the If this value is non-zero, then for the node-to-surface and surface-to-surface contacts LS-DYNA performs a special check at time 0 for elements that are inverted (or nearly so), These elements are removed from contact.  These poorly formed elements have been known to occur on the tooling in metalforming problems, which allows these problems to run.  It should not normally be needed for reasonable meshes.
          * - :py:attr:`~pensf`
            - Get or set the This option is used together with IGNORE for 3D forging problems.  If non-zero, the IGNORE penetration distance is multiplied by this value each cycle, effectively pushing the tracked node back out to the surface.  This is useful for nodes that might get generated below the reference surface during 3D remeshing.  Care should be exercised, as energy may be generated and stability may be effected for values lower than 0.95.  A value in the range of 0.98 to 0.99 or higher (but < 1.0) is recommended
          * - :py:attr:`~grpable`
            - Get or set the Set to 1 to invoke an alternate MPP communication algorithm for various SINGLE_SURFACE (including AUTOMATIC_GEN-ERAL), NODES_TO_SURFACE, SURFACE_TO_SURFACE, ERODING and SOFT = 2 contacts.  This groupable algorithm does not support all contact options, including MORTAR. It is still under development.  It can be significantly faster and scale better than the normal algorithm when there are more than two or three applicable contact types defined in the model. It is intended for speeding up the contact processing without changing the behavior of the contact.  See also *CONTROL_MPP_-CONTACT_GROUPABLE.
          * - :py:attr:`~soft`
            - Get or set the Soft constraint option:
          * - :py:attr:`~sofscl`
            - Get or set the Scale factor for constraint forces of soft constraint option invoked with SOFT = 1(default=.10). Values greater than .5 for single surface contact and 1.0 for a one way treatment are inadmissible.
          * - :py:attr:`~lcidab`
            - Get or set the Load curve ID defining airbag thickness as a function of time for type a13 contact (*CONTACT_AIRBAG_SINGLE_SURFACE).
          * - :py:attr:`~maxpar`
            - Get or set the Maximum parametric coordinate in segment search (values 1.025 and 1.20 recommended). Larger values can increase cost. If zero, the default is set to 1.025. This factor allows an increase in the size of the segments . May be useful at sharp corners.
          * - :py:attr:`~sbopt`
            - Get or set the Segment-based contact options (SOFT=2).
          * - :py:attr:`~depth`
            - Get or set the Search depth in automatic contact. Value of 1 is sufficiently accurate for most crash applications and is much less expensive. LS-DYNA for improved accuracy sets this value to 2. If zero, the default is set to 2.
          * - :py:attr:`~bsort`
            - Get or set the Number of cycles between bucket sorts.  Values of 25 and 100 are recommended for contact types 4 (SINGLE_SURFACE) and 13 (AUTOMATIC_SINGLE_SURFACE), respectively.  Values of 10-15 are okay for surface-to-surface and node-to-surface contact.  If zero, LS-DYNA determines the interval.  BSORT applies only to SMP (see BCKT on MPP 1 for MPP) except in the case of SOFT = 2 or for Mortar contact, in which case BSORT applies to both SMP and MPP. For Mortar contact the default is the value associated with NSBCS on *CONTROL_CONTACT.
          * - :py:attr:`~frcfrq`
            - Get or set the Number of cycles between contact force updates for penalty contact formulations. This option can provide a significant speed-up of the contact treatment. If used, values exceeding 3 or 4 are dangerous. Considerable care must be exercised when using this option, as this option assumes that contact does not change FRCFRG cycles.
          * - :py:attr:`~penmax`
            - Get or set the For old types 3, 5, 8, 9, 10 (see Mapping of *CONTACT keyword option to contact type in d3hsp at the end of General Remarks) and Mortar contact, PENMAX is the maximum penetration distance. For contact types a3, a5, a10, 13, 15, and 26, the segment thickness multiplied by PENMAX defines the maximum penetration allowed (as a multiple of the segment thickness).  (See Table 0-2.):):
          * - :py:attr:`~thkopt`
            - Get or set the Thickness option for contact types 3, 5, and 10:
          * - :py:attr:`~shlthk`
            - Get or set the Define if and only if THKOPT above equals 1. Shell thickness considered in type surface to surface and node to surface type contact options, where options 1 and 2 below activate the new contact algorithms. The thickness offsets are always included in single surface and constraint method contact types:
          * - :py:attr:`~snlog`
            - Get or set the Disable shooting node logic in thickness offset contact. With the shooting node logic enabled, the first cycle that a tracked node penetrates a reference segment, that node is moved back to the reference surface without applying any contact force.
          * - :py:attr:`~isym`
            - Get or set the Symmetry plane option:
          * - :py:attr:`~i2d3d`
            - Get or set the Segment searching option:
          * - :py:attr:`~sldthk`
            - Get or set the Optional solid element thickness. A nonzero positive value will activate the contact thickness offsets in the contact algorithms where offsets apply. The contact treatment with then be equivalent to the case where null shell elements are used to cover the brick elements. The contact stiffness parameter below, SLDSTF, may also be used to override the default value.
          * - :py:attr:`~sldstf`
            - Get or set the Optional solid element stiffness. A nonzero positive value overrides the bulk modulus taken from the material model referenced by the solid element.
          * - :py:attr:`~igap`
            - Get or set the For mortar contact IGAP is used to progressively increase contact stiffness for large penetrations, or use a linear relationship between penetration and contact pressure; see remarks on mortar contact below.
          * - :py:attr:`~dprfac`
            - Get or set the Applies to the SOFT=2 and Mortar contacts. Depth of penetration reduction factor for SOFT=2 contact.
          * - :py:attr:`~dtstif`
            - Get or set the Applies to the SOFT=1 and SOFT=2 and Mortar contacts. Time step used in stiffness calculation for SOFT=1 and SOFT=2 contact.
          * - :py:attr:`~edgek`
            - Get or set the Scale factor for penalty stiffness of edge to edge contact when SOFT = 2 and DEPTH = 5, 15, 25, or 35:
          * - :py:attr:`~flangl`
            - Get or set the Angle tolerance in radians for feature lines option in smooth contact.
          * - :py:attr:`~cid_rcf`
            - Get or set the Coordinate system ID to output RCFORC force resultants in a local system.
          * - :py:attr:`~q2tri`
            - Get or set the Option to split quadrilateral contact segments into two triangles (only available when SOFT=2).
          * - :py:attr:`~dtpchk`
            - Get or set the Time interval between shell penetration reports (only available for segment based contact)
          * - :py:attr:`~sfnbr`
            - Get or set the Scale factor for neighbor segment contact (only available for segment based contact)
          * - :py:attr:`~fnlscl`
            - Get or set the Scale factor for nonlinear force scaling
          * - :py:attr:`~dnlscl`
            - Get or set the Distance for nonlinear force scaling
          * - :py:attr:`~tcso`
            - Get or set the Option to consider only contact segments (not all attached elements) when
          * - :py:attr:`~tiedid`
            - Get or set the Incremental displacement update for tied contacts.EQ.0:  Off (default).
          * - :py:attr:`~shledg`
            - Get or set the Flag for assuming edge shape for shells when measuring penetration.This is available for segment - based contact(SOFT = 2).
          * - :py:attr:`~sharec`
            - Get or set the Shared constraint flag (only available for segment based contact)
          * - :py:attr:`~ipback`
            - Get or set the If set to a nonzero value, creates a  backup  penalty tied contact for this
          * - :py:attr:`~srnde`
            - Get or set the Segment Rounded Edges:
          * - :py:attr:`~fricsf`
            - Get or set the Scale factor for frictional stiffness (available for SOFT = 2 only).
          * - :py:attr:`~icor`
            - Get or set the If set to a nonzero value, VDC is the coefficient of restitution
          * - :py:attr:`~ftorq`
            - Get or set the If set to 1, a torsional force is computed in the beam to beam portion
          * - :py:attr:`~region`
            - Get or set the The ID of a *DEFINE_REGION which will delimit the volume of
          * - :py:attr:`~pstiff`
            - Get or set the Flag to choose the method for calculating the penalty stiffness. This is available for segment based contact (see SOFT on optional card A)
          * - :py:attr:`~ignroff`
            - Get or set the Flag to ignore the thickness offset for shells in the calculation of the shell contact penetration depth. This allows shells to be used for
          * - :py:attr:`~fstol`
            - Get or set the Tolerance used with the SMOOTH option for determining which segments are considered flat.  The value is in degrees and approximately represents half the angle between adjacent segments
          * - :py:attr:`~ssftyp`
            - Get or set the Flag to determine how the SSF option on *PART_CONTACT behaves when SOFT = 2 on optional card A:
          * - :py:attr:`~swtpr`
            - Get or set the Flag to use tapered shell contact segments adjacent to segments that are thinned by the SPOTHIN option on *CONTROL_CONTACT. This option is only available when SOFT=2 on optional card A.
          * - :py:attr:`~tetfac`
            - Get or set the Scale factor for the computed volume of tetrahedral solid elements for the mass calculation in SOFT=2 contact. By default, half the mass of a solid element is considered for the contact segment, which is reasonable for hexahedrons. In contrast, for tetrahedrons, a larger value than 0.5 would be preferrable, because several tets fit into one hex. Therefore, a TETFAC value around 3.0 to 5.0 should make the contact stiffness more comparable with hex meshes.
          * - :py:attr:`~shloff`
            - Get or set the Flag affecting the location of the contact surfaces for shells when NLOC is nonzero in *SECTION_SHELL or *PART_COMPOSITE, or when OFFSET is specified using *ELEMENT_SHELL_OFFSET. Thus, set this field to 1 to enable the behavior locally for this contact and leave CNTCO as 0 to disable this behavior for all contacts without this field set to 1.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from contact_gebod_right_foot import ContactGebodRightFoot

Property detail
---------------

.. py:property:: did
   :type: Optional[int]


   
   Get or set the Dummy ID, see *COMPONENT_GEBOD.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Slave set ID, see *SET_NODE_OPTION, *PART, or *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: sstyp
   :type: int


   
   Get or set the Slave set type:
   EQ.0: node set (default),
   EQ.1: part ID,
   EQ.2: part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Penalty scale factor. Useful to scale maximized penalty (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: df
   :type: float


   
   Get or set the Damping option, see description for *CONTACT_OPTION:
   EQ.0.0: no damping,
   GT.0.0: viscous damping in percent of critical, e.g., 20 for 20% damping (default=20.0),
   EQ.-n: |n| is the load curve ID giving the damping force versus relative normal velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cf
   :type: float


   
   Get or set the Coulomb friction coefficient. Assumed to be constant (default=0.5).
















   ..
       !! processed by numpydoc !!

.. py:property:: intord
   :type: int


   
   Get or set the Integration order (slaved materials only).
   EQ.0: check nodes only (default),
   EQ.1: 1 point integration over segments,
   EQ.2: 2x2 integration,
   EQ.3: 3x3 integration,
   EQ.4: 4x4 integration,
   EQ.5: 5x5 integration.
   This option allows a check of the penetration of the dummy segment into the deformable (slaved) material. Then virtual nodes at the location of the integration points are checked.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time (default=1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: so
   :type: int


   
   Get or set the Flag to use penalty stiffness as in surface to surface contact:
   EQ.0: contact entity stiffness formulation (default),
   EQ.1: surface to surface contact method,
   EQ.-n: |n| is the load curve ID giving the force versus the normal penetration.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the ID keyword option
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the Interface descriptor. We suggest using unique descriptions.
















   ..
       !! processed by numpydoc !!

.. py:property:: ignore
   :type: int


   
   Get or set the By setting this variable to 1, the "ignore initial penetrations" option is turned on for this contact.  Alternatively, this option may be turned on by setting IGNORE = 1 on Card 4 of *CONTROL_CONTACT or on Optional Card C of *CONTACT.  In other words, if IGNORE is set to 1 in any of three places, initial penetrations are tracked.
















   ..
       !! processed by numpydoc !!

.. py:property:: bckt
   :type: int


   
   Get or set the Bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts. For these two exceptions, the BSORT option on Optional Card A applies instead.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcbckt
   :type: Optional[int]


   
   Get or set the Load curve for bucket sort frequency. This parameter does not apply when SOFT = 2 on Optional Card A or to Mortar contacts.  For the two exceptions, the negative BSORT option on Optional Card A applies instead.
















   ..
       !! processed by numpydoc !!

.. py:property:: ns2trk
   :type: int


   
   Get or set the Number of potential contacts to track for each tracked node.  The normal input for this (DEPTH on Optional Card A) is ignored..
















   ..
       !! processed by numpydoc !!

.. py:property:: inititr
   :type: int


   
   Get or set the Number of iterations to perform when trying to eliminate initial penetrations.  Note that an input of 0 means 0, not the default value (which is 2).  Leaving this field blank will set INITITR to 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: parmax
   :type: float


   
   Get or set the The parametric extension distance for contact segments.  The MAXPAR parameter on Optional Card A is not used for MPP.  For non-tied contacts, the default is 1.0005. For tied contacts the default is 1.035 and, the actual extension used is computed as follows: see the manual
















   ..
       !! processed by numpydoc !!

.. py:property:: cparm8
   :type: int


   
   Get or set the Flag for behavior of AUTOMATIC_GENERAL contacts.  CPARM8's value is interpreted as two separate flags: OPT1 and OPT2 according to the rule,
   "CPARM8" = "OPT1" + "OPT2".
   When OPT1 and OPT2 are both set, both options are active.

   OPT1.Flag to exclude beam - to - beam contact from the same PID.
   EQ.0:   Flag is not set(default).
   EQ.1 : Flag is set.
   EQ.2 : Flag is set.CPARM8 = 2 additionally permits contact treatment of spot weld(type 9) beams in AUTOMATIC_GENERAL contacts; spot weld beams are otherwise disregarded entirely by AUTOMATIC_GENERAL contacts.
   OPT2.Flag to shift generated beam affecting only shell - edge - to - shell - edge treatment.See also SRNDE in Optional Card E.
   EQ.10:  Beam generated on exterior shell edge will be shifted into the shell by half the shell thickness.Therefore, the shell - edge - to - shell - edge contact starts right at the shell edge and not at an extension of the shell edge.















   ..
       !! processed by numpydoc !!

.. py:property:: mpp2
   :type: bool


   
   Get or set the Flag whether this is the MPP card.
















   ..
       !! processed by numpydoc !!

.. py:property:: chksegs
   :type: int


   
   Get or set the If this value is non-zero, then for the node-to-surface and surface-to-surface contacts LS-DYNA performs a special check at time 0 for elements that are inverted (or nearly so), These elements are removed from contact.  These poorly formed elements have been known to occur on the tooling in metalforming problems, which allows these problems to run.  It should not normally be needed for reasonable meshes.
















   ..
       !! processed by numpydoc !!

.. py:property:: pensf
   :type: float


   
   Get or set the This option is used together with IGNORE for 3D forging problems.  If non-zero, the IGNORE penetration distance is multiplied by this value each cycle, effectively pushing the tracked node back out to the surface.  This is useful for nodes that might get generated below the reference surface during 3D remeshing.  Care should be exercised, as energy may be generated and stability may be effected for values lower than 0.95.  A value in the range of 0.98 to 0.99 or higher (but < 1.0) is recommended
















   ..
       !! processed by numpydoc !!

.. py:property:: grpable
   :type: int


   
   Get or set the Set to 1 to invoke an alternate MPP communication algorithm for various SINGLE_SURFACE (including AUTOMATIC_GEN-ERAL), NODES_TO_SURFACE, SURFACE_TO_SURFACE, ERODING and SOFT = 2 contacts.  This groupable algorithm does not support all contact options, including MORTAR. It is still under development.  It can be significantly faster and scale better than the normal algorithm when there are more than two or three applicable contact types defined in the model. It is intended for speeding up the contact processing without changing the behavior of the contact.  See also *CONTROL_MPP_-CONTACT_GROUPABLE.
















   ..
       !! processed by numpydoc !!

.. py:property:: soft
   :type: Optional[int]


   
   Get or set the Soft constraint option:
   EQ.0: Standard penalty formulation,
   EQ.1: soft constraint penalty formulation,
   EQ.2: pinball segment based contact penalty formulation.
   EQ.4: Constraint approach for FORMING contacts. This formulation only applies to one-way forming contacts. You should use it when the penalty formulations result in large penetrations. The results, however, are sensitive to damping.
   EQ.6:Special contact algorithm to handle sheet blank edge(deformable) to gage pin(rigid shell) contact during implicit gravity loading.This applies to * CONTACT_FORMING_NODES_TO_SURFACE only.See remarks under About SOFT = 6
















   ..
       !! processed by numpydoc !!

.. py:property:: sofscl
   :type: float


   
   Get or set the Scale factor for constraint forces of soft constraint option invoked with SOFT = 1(default=.10). Values greater than .5 for single surface contact and 1.0 for a one way treatment are inadmissible.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidab
   :type: int


   
   Get or set the Load curve ID defining airbag thickness as a function of time for type a13 contact (*CONTACT_AIRBAG_SINGLE_SURFACE).
















   ..
       !! processed by numpydoc !!

.. py:property:: maxpar
   :type: float


   
   Get or set the Maximum parametric coordinate in segment search (values 1.025 and 1.20 recommended). Larger values can increase cost. If zero, the default is set to 1.025. This factor allows an increase in the size of the segments . May be useful at sharp corners.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbopt
   :type: int


   
   Get or set the Segment-based contact options (SOFT=2).
   EQ.0: defaults to 2.
   EQ.1: pinball edge-edge contact (not recommended).
   EQ.2: assume planer segments (default).
   EQ.3: warped segment checking.
   EQ.4: sliding option,
   EQ.5: do options 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: depth
   :type: int


   
   Get or set the Search depth in automatic contact. Value of 1 is sufficiently accurate for most crash applications and is much less expensive. LS-DYNA for improved accuracy sets this value to 2. If zero, the default is set to 2.
   LT.0: |DEPTH| is the load curve ID defining searching depth versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsort
   :type: Optional[int]


   
   Get or set the Number of cycles between bucket sorts.  Values of 25 and 100 are recommended for contact types 4 (SINGLE_SURFACE) and 13 (AUTOMATIC_SINGLE_SURFACE), respectively.  Values of 10-15 are okay for surface-to-surface and node-to-surface contact.  If zero, LS-DYNA determines the interval.  BSORT applies only to SMP (see BCKT on MPP 1 for MPP) except in the case of SOFT = 2 or for Mortar contact, in which case BSORT applies to both SMP and MPP. For Mortar contact the default is the value associated with NSBCS on *CONTROL_CONTACT.
   LT.0: |BSORT| is the load curve ID defining bucket sorting frequency as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: frcfrq
   :type: int


   
   Get or set the Number of cycles between contact force updates for penalty contact formulations. This option can provide a significant speed-up of the contact treatment. If used, values exceeding 3 or 4 are dangerous. Considerable care must be exercised when using this option, as this option assumes that contact does not change FRCFRG cycles.
   EQ.0: FRCFRG is set to 1 and force calculations are performed each cycle-strongly recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: penmax
   :type: float


   
   Get or set the For old types 3, 5, 8, 9, 10 (see Mapping of *CONTACT keyword option to contact type in d3hsp at the end of General Remarks) and Mortar contact, PENMAX is the maximum penetration distance. For contact types a3, a5, a10, 13, 15, and 26, the segment thickness multiplied by PENMAX defines the maximum penetration allowed (as a multiple of the segment thickness).  (See Table 0-2.):):
   EQ.0.0 for old type contacts 3, 5, and 10: Use small penetration search and value calculated from thickness and XPENE, see *CONTROL_ CONTACT.
   EQ.0.0 for contact types a 3, a 5, a10, 13, and 15: Default is 0.4, or 40 percent of the segment thickness
   EQ.0.0 for contact type26: Default is 200.0 times the segment thickness
















   ..
       !! processed by numpydoc !!

.. py:property:: thkopt
   :type: int


   
   Get or set the Thickness option for contact types 3, 5, and 10:
   EQ.0: default is taken from control card, *CONTROL_CONTACT,
   EQ.1: thickness offsets are included,
   EQ.2: thickness offsets are not included (old way).
















   ..
       !! processed by numpydoc !!

.. py:property:: shlthk
   :type: int


   
   Get or set the Define if and only if THKOPT above equals 1. Shell thickness considered in type surface to surface and node to surface type contact options, where options 1 and 2 below activate the new contact algorithms. The thickness offsets are always included in single surface and constraint method contact types:
   EQ.0: thickness is not considered,
   EQ.1: thickness is considered but rigid bodies are excluded,
   EQ.2: thickness is considered including rigid bodies.
















   ..
       !! processed by numpydoc !!

.. py:property:: snlog
   :type: int


   
   Get or set the Disable shooting node logic in thickness offset contact. With the shooting node logic enabled, the first cycle that a tracked node penetrates a reference segment, that node is moved back to the reference surface without applying any contact force.
   EQ.0: logic is enabled (default),
   EQ.1: logic is skipped (sometimes recommended for metalforming calculations).
















   ..
       !! processed by numpydoc !!

.. py:property:: isym
   :type: int


   
   Get or set the Symmetry plane option:
   EQ.0: off,
   EQ.1: do not include faces with normal boundary constraints (e.g., segments of brick elements on a symmetry plane).
   This option is important to retain the correct boundary conditions in the model with symmetry. For the _ERODING_ contacts this option may also be defined on card 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: i2d3d
   :type: int


   
   Get or set the Segment searching option:
   EQ.0: search 2D elements (shells) before 3D elements (solids, thick shells) when locating segments.
   EQ.1: search 3D (solids, thick shells) elements before 2D elements (shells) when locating segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: sldthk
   :type: float


   
   Get or set the Optional solid element thickness. A nonzero positive value will activate the contact thickness offsets in the contact algorithms where offsets apply. The contact treatment with then be equivalent to the case where null shell elements are used to cover the brick elements. The contact stiffness parameter below, SLDSTF, may also be used to override the default value.
















   ..
       !! processed by numpydoc !!

.. py:property:: sldstf
   :type: float


   
   Get or set the Optional solid element stiffness. A nonzero positive value overrides the bulk modulus taken from the material model referenced by the solid element.
















   ..
       !! processed by numpydoc !!

.. py:property:: igap
   :type: int


   
   Get or set the For mortar contact IGAP is used to progressively increase contact stiffness for large penetrations, or use a linear relationship between penetration and contact pressure; see remarks on mortar contact below.
   For other contacts it is a flag to improve implicit convergence behavior
   at the expense of (1) creating some sticking if parts attempt to separate
   and (2) possibly underreporting the contact force magnitude in the
   output files rcforc and ncforc. (IMPLICIT ONLY.).
   LT.0: Like IGAP = 1 except the maximum distance between contact surfaces at which stickiness is on is sacled by IGAP/10.
   EQ.1: Apply method to improve convergence (DEFAULT)
   EQ.2: Do not apply method
   GT.2: Set IGAP = 1 for first IGAP-2 converged equilibrium states,
















   ..
       !! processed by numpydoc !!

.. py:property:: dprfac
   :type: float


   
   Get or set the Applies to the SOFT=2 and Mortar contacts. Depth of penetration reduction factor for SOFT=2 contact.
   EQ.0.0:Initial penetrations are always ignored.
   GT.0.0: Initial penetrations are penalized over time.
   LT.0.0:|DPRFAC| is the load curve ID defining DPRFAC versus time.
   For the mortar conatact MPAR1 corresponds to initial contact pressure in interfaces with initial penetrations if IGNORE=2, for IGNORE=3,4 it corresponds to the time of closure of initial penetrations.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtstif
   :type: float


   
   Get or set the Applies to the SOFT=1 and SOFT=2 and Mortar contacts. Time step used in stiffness calculation for SOFT=1 and SOFT=2 contact.
   EQ.0.0:Use the initial value that is used for time integration.
   GT.0.0: Use the value specified.
   LT.-0.01 and GT.-1.0: use a moving average of the solution time step. (SOFT=2 only).
   LT.-1.0: |DTSTIF| is the load curve ID defining DTSTIF versus time.
   For the mortar contact and IGNORE=4, MPAR2 corresponds a penetration depth that must be at least the penetration occurring in the contact interface.
















   ..
       !! processed by numpydoc !!

.. py:property:: edgek
   :type: float


   
   Get or set the Scale factor for penalty stiffness of edge to edge contact when SOFT = 2 and DEPTH = 5, 15, 25, or 35:
   EQ.0.0: Use the default penalty stiffness.
   GT.0.0: Scale the stiffness by EDGEK.
















   ..
       !! processed by numpydoc !!

.. py:property:: flangl
   :type: float


   
   Get or set the Angle tolerance in radians for feature lines option in smooth contact.
   EQ.0.0:No feature line is considered for surface fitting in smooth contact.
   GT.0.0:Any edge with angle between two contact segments bigger than this angle will be treated as feature line during surface fitting in smooth contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid_rcf
   :type: Optional[int]


   
   Get or set the Coordinate system ID to output RCFORC force resultants in a local system.
















   ..
       !! processed by numpydoc !!

.. py:property:: q2tri
   :type: int


   
   Get or set the Option to split quadrilateral contact segments into two triangles (only available when SOFT=2).
   EQ.0:Off (default).
   EQ.1:On for all SURFA shell segments.
   EQ.2:On for all SURFB shell segments.
   EQ.3:On for all shell segments.
   EQ.4:On for all shell segments of material type 34.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtpchk
   :type: float


   
   Get or set the Time interval between shell penetration reports (only available for segment based contact)
   EQ.0.0:Off (default).
   GT.0.0:  Check and report segment penetrations at time intervals equal to DTPCHK.
   LT.0.0:Check and report segment penetrations at time intervals equal to |DTPCHK|. In addition, calculation stops with an error at t=0 if any intersections are initially present
















   ..
       !! processed by numpydoc !!

.. py:property:: sfnbr
   :type: float


   
   Get or set the Scale factor for neighbor segment contact (only available for segment based contact)
   EQ.0.0:Off (default).
   GT.0.0:  Check neighbor segments for contact
















   ..
       !! processed by numpydoc !!

.. py:property:: fnlscl
   :type: float


   
   Get or set the Scale factor for nonlinear force scaling
















   ..
       !! processed by numpydoc !!

.. py:property:: dnlscl
   :type: float


   
   Get or set the Distance for nonlinear force scaling
















   ..
       !! processed by numpydoc !!

.. py:property:: tcso
   :type: int


   
   Get or set the Option to consider only contact segments (not all attached elements) when
   computing the contact thickness for a node or segment (for SURFACE_TO_SURFACE contact and shell elements only)
   EQ.0: Off (default).
   EQ.1: Only consider segments in the contact definition
















   ..
       !! processed by numpydoc !!

.. py:property:: tiedid
   :type: int


   
   Get or set the Incremental displacement update for tied contacts.EQ.0:  Off (default).
   EQ.1:  On.
















   ..
       !! processed by numpydoc !!

.. py:property:: shledg
   :type: int


   
   Get or set the Flag for assuming edge shape for shells when measuring penetration.This is available for segment - based contact(SOFT = 2).
   EQ.0:Default to SHELDG on * CONTROL_CONTACT
   EQ.1 : Shell edges are assumed to be square and are flush with the nodes.
   EQ.2 : Shell edges are assumed to be round with a radius equal to half the shell thickness.The edge centers lie on the lines between the segment nodes and extend outward by the radius.This option is not available for DEPTH values of 23, 33, or 35.
















   ..
       !! processed by numpydoc !!

.. py:property:: sharec
   :type: int


   
   Get or set the Shared constraint flag (only available for segment based contact)
   EQ.0: Segments that share constraints not checked for contact.
   EQ.1: Segments that share constraints are checked for contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipback
   :type: int


   
   Get or set the If set to a nonzero value, creates a  backup  penalty tied contact for this
   interface. This option applies to constrained tied contacts only. See Remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: srnde
   :type: int


   
   Get or set the Segment Rounded Edges:
   EQ.0: free edges have their usual treatement
   EQ.1: free edges are rounded, but without extending them.
















   ..
       !! processed by numpydoc !!

.. py:property:: fricsf
   :type: float


   
   Get or set the Scale factor for frictional stiffness (available for SOFT = 2 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: icor
   :type: int


   
   Get or set the If set to a nonzero value, VDC is the coefficient of restitution
   expressed as a percentage. When SOFT = 0 or 1, this option applies
   to AUTOMATIC_NODES_TO_SURFACE, AUTOMATIC_SURFACE_TO_SURFACE and AUTOMATIC_SINGLE_SURFACE.
   When SOFT = 2, it applies to all available keywords.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftorq
   :type: int


   
   Get or set the If set to 1, a torsional force is computed in the beam to beam portion
   of contact type AUTOMATIC_GENERAL, which balances the
   torque produced due to friction. This is currently only available in the MPP version.
















   ..
       !! processed by numpydoc !!

.. py:property:: region
   :type: int


   
   Get or set the The ID of a *DEFINE_REGION which will delimit the volume of
   space where this contact is active. See Remark 4 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: pstiff
   :type: int


   
   Get or set the Flag to choose the method for calculating the penalty stiffness. This is available for segment based contact (see SOFT on optional card A)
   EQ.0: Use the default as defined by PSTIFF on *CONTROL_CONTACT.
   EQ.1: Based on nodal masses
   EQ.2: Based on material density and segment dimensions.
















   ..
       !! processed by numpydoc !!

.. py:property:: ignroff
   :type: int


   
   Get or set the Flag to ignore the thickness offset for shells in the calculation of the shell contact penetration depth. This allows shells to be used for
   meshing rigid body dies without modifying the positions of the nodes to compensate for the shell thickness.
   EQ.0: Default
   EQ.1: Ignore the SURFB side thickness.
   EQ.2: Ignore the SURFA side thickness.
   EQ.3: Ignore the thickness of both sides..
















   ..
       !! processed by numpydoc !!

.. py:property:: fstol
   :type: float


   
   Get or set the Tolerance used with the SMOOTH option for determining which segments are considered flat.  The value is in degrees and approximately represents half the angle between adjacent segments
















   ..
       !! processed by numpydoc !!

.. py:property:: ssftyp
   :type: int


   
   Get or set the Flag to determine how the SSF option on *PART_CONTACT behaves when SOFT = 2 on optional card A:
   EQ.0:Use SSF from the tracked segment as determined by the SOFT = 2 algorithm (see Remark 2)
   EQ.1 : Use the larger of the SSF values.
















   ..
       !! processed by numpydoc !!

.. py:property:: swtpr
   :type: int


   
   Get or set the Flag to use tapered shell contact segments adjacent to segments that are thinned by the SPOTHIN option on *CONTROL_CONTACT. This option is only available when SOFT=2 on optional card A.
   EQ.0:Use full thickness constant segments.
   EQ.1 : Use tapered segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: tetfac
   :type: float


   
   Get or set the Scale factor for the computed volume of tetrahedral solid elements for the mass calculation in SOFT=2 contact. By default, half the mass of a solid element is considered for the contact segment, which is reasonable for hexahedrons. In contrast, for tetrahedrons, a larger value than 0.5 would be preferrable, because several tets fit into one hex. Therefore, a TETFAC value around 3.0 to 5.0 should make the contact stiffness more comparable with hex meshes.
















   ..
       !! processed by numpydoc !!

.. py:property:: shloff
   :type: float


   
   Get or set the Flag affecting the location of the contact surfaces for shells when NLOC is nonzero in *SECTION_SHELL or *PART_COMPOSITE, or when OFFSET is specified using *ELEMENT_SHELL_OFFSET. Thus, set this field to 1 to enable the behavior locally for this contact and leave CNTCO as 0 to disable this behavior for all contacts without this field set to 1.
   EQ.0: The setting of CNTO on *CONTROL_SHELL determines the contact reference plane.
   EQ.1:The contact reference plance coincides with shell reference surface.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'GEBOD_RIGHT_FOOT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





