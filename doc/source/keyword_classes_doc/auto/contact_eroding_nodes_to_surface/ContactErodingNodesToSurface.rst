





:class:`ContactErodingNodesToSurface`
=====================================


.. py:class:: contact_eroding_nodes_to_surface.ContactErodingNodesToSurface(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_ERODING_NODES_TO_SURFACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactErodingNodesToSurface

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfa`
            - Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface (see Setting the Contact Interface). See *SET_SEGMENT, *SET_NODE_OPTION, *PART, *SET_PART or *SET_SHELL_OPTION. For ERODING_SINGLE_SURFACE and ERODING_SURFACE_TO_SURFACE contact types, use either a part ID or a part set ID. For ERODING_NODES_TO_SURFACE contact, use a node set which includes all nodes that may be exposed to contact as element erosion occurs.
          * - :py:attr:`~surfb`
            - Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact (see Setting the Contact Interface).
          * - :py:attr:`~surfatyp`
            - Get or set the The ID type of SURFA:
          * - :py:attr:`~surfbtyp`
            - Get or set the ID type of SURFB:
          * - :py:attr:`~saboxid`
            - Get or set the Include in contact definition only those SURFA nodes/segments within box SABOXID (corresponding to BOXID in *DEFINE_BOX), or if SABOXID is negative, only those SURFA nodes/segments within contact volume |SABOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SABOXID can be used only if SURFATYP is set to 2, 3, or 6, that is, SURFA is a part ID or part set ID. SABOXID is not available for ERODING contact types
          * - :py:attr:`~sbboxid`
            - Get or set the Include in contact definition only those SURFB segments within box SBBOXID (corresponding to BOXID in *DEFINE_BOX), or if SBBOXID is negative, only those SURFB segments within contact volume |SBBOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SBBOXID can be used only if SURFBTYP is set to 2, 3, or 6, that is, SURFB is a part ID or part set ID.  SBBOXID is not available for ERODING contact types.
          * - :py:attr:`~sapr`
            - Get or set the Include the SURFA side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
          * - :py:attr:`~sbpr`
            - Get or set the Include the SURFB side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
          * - :py:attr:`~fs`
            - Get or set the Static coefficient of friction if FS > 0 and not equal to 2.
          * - :py:attr:`~fd`
            - Get or set the Dynamic coefficient of friction. The frictional coefficient is assumed to be dependent on the relative velocity v-rel of the surfaces in contact. Give table ID if FS=2 (default=0.0).
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity v-rel of the surfaces in contact. (default=0.0).
          * - :py:attr:`~vc`
            - Get or set the Coefficient for viscous friction. This is necessary to limit the friction force to a maximum.
          * - :py:attr:`~vdc`
            - Get or set the Viscous damping coefficient in percent of critical. In order to avoid undesirable oscillation in contact, e.g., for sheet forming simulation, a contact damping perpendicular to the contacting surfaces is applied.
          * - :py:attr:`~penchk`
            - Get or set the Small penetration in contact search option.  If the tracked node penetrates more than the segment thickness times the factor XPENE (see *CONTROL_CONTACT), the penetration is ignored, and the tracked node is set free.  The thickness is taken as the shell thickness if the segment belongs to a shell element or it is taken as 1/20 of its shortest diagonal if the segment belongs to a solid element.  This option applies to the surface-to-surface contact algorithms.  See Table 0-17 for contact types and more details.
          * - :py:attr:`~bt`
            - Get or set the Birth time (contact surface becomes active at this time):LT.0:   Birth time is set to | "BT" | .When negative, birth time is followed during the dynamic relaxation phase of the calculation.After dynamic relaxation has completed, contact is activated regardless of the value of BT.EQ.0 : Birth time is inactive, meaning contact is always activeGT.0 : If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth - time / death - time; see Remark 2 below.Otherwise, if "DT" > 0, birth time applies both duringand after dynamic relaxation.
          * - :py:attr:`~dt`
            - Get or set the Death time (contact surface is deactivated at this time):LT.0:   If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth - time / death - time.Otherwise, negative DT indicates that contact is inactive during dynamic relaxation.After dynamic relaxation the birth and death times are followed and set to | "BT" | and | "DT" | , respectively.EQ.0 : DT defaults to 10e20.GT.0 : DT sets the time at which the contact is deactivated.
          * - :py:attr:`~sfsa`
            - Get or set the Scale factor on default SURFA penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.For MORTAR frictional contact this is the stiffness scale factor for the entire contact, and SFSB does not apply.
          * - :py:attr:`~sfsb`
            - Get or set the Scale factor on default SURFA penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.For MORTAR tied contact, this is an additional stiffness scale factor, resulting in a total stiffness scale of SFSA*SFSB.
          * - :py:attr:`~sast`
            - Get or set the Optional thickness for SURFA surface (overrides true thickness). This option applies only to contact with shell elements. SAST has no bearing on the actual thickness of the elements; it only affects the location of the contact surface. For the *CONTACT_TIED_.. options, SAST and SBST below can be defined as negative values, which will cause the determination of whether or not a node is tied to depend only on the separation distance relative to the absolute value of these thicknesses. More information is given under General Remarks on *CONTACT following Optional Card C.
          * - :py:attr:`~sbst`
            - Get or set the Optional thickness for SURFA surface (overrides true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements. For the TIED options see SAST above.
          * - :py:attr:`~sfsat`
            - Get or set the Scale factor applied to contact thickness of SURFA surface.  This option applies to contact with shell and beam elements.
          * - :py:attr:`~sfsbt`
            - Get or set the Scale factor applied to contact thickness of SURFA surface.  This option applies only to contact with shell elements.
          * - :py:attr:`~fsf`
            - Get or set the Coulomb friction scale factor (default=1.0).The Coulomb friction value is scaled as μ_sc=FSF×μ_c; see Mandatory Card 2.
          * - :py:attr:`~vsf`
            - Get or set the Viscous friction scale factor (default=1.0).If this factor is defined, then the limiting force becomes: F_lim =VSF×VC×A_cont ; see Mandatory Card 2.
          * - :py:attr:`~isym`
            - Get or set the Symmetry plane option:
          * - :py:attr:`~erosop`
            - Get or set the Erosion/Interior node option:
          * - :py:attr:`~iadj`
            - Get or set the Adjacent material treatment for solid elements:
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

    from contact_eroding_nodes_to_surface import ContactErodingNodesToSurface

Property detail
---------------

.. py:property:: surfa
   :type: Optional[int]


   
   Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface (see Setting the Contact Interface). See *SET_SEGMENT, *SET_NODE_OPTION, *PART, *SET_PART or *SET_SHELL_OPTION. For ERODING_SINGLE_SURFACE and ERODING_SURFACE_TO_SURFACE contact types, use either a part ID or a part set ID. For ERODING_NODES_TO_SURFACE contact, use a node set which includes all nodes that may be exposed to contact as element erosion occurs.
   EQ.0:   Includes all parts in the case of single surface contact types
















   ..
       !! processed by numpydoc !!

.. py:property:: surfb
   :type: Optional[int]


   
   Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact (see Setting the Contact Interface).
   EQ.0:   SURFB side is not applicable for single surface contact types.
















   ..
       !! processed by numpydoc !!

.. py:property:: surfatyp
   :type: int


   
   Get or set the The ID type of SURFA:
   EQ.0: segment set ID for surface to surface contact,
   EQ.1: shell element set ID for surface to surface contact,
   EQ.2: part set ID,
   EQ.3: part ID,
   EQ.4: node set ID for node to surface contact,
   EQ.5: include all (SURFA field) is ignored,
   EQ.6: part set ID for exempted parts. All non-exempted parts are included in the contact.
   EQ.7:   Branch ID; see *SET_PART_TREE
















   ..
       !! processed by numpydoc !!

.. py:property:: surfbtyp
   :type: int


   
   Get or set the ID type of SURFB:
   EQ.0: segment set ID,
   EQ.1: shell element set ID,
   EQ.2: part set ID,
   EQ.3: part ID,
   EQ.5:Include all ( SURFB Field is ignored).
   EQ.6:   Part set ID for exempted parts.  All non-exempted parts are included in the contact.
   EQ.7:   Branch ID; see *SET_PART_TREE
















   ..
       !! processed by numpydoc !!

.. py:property:: saboxid
   :type: Optional[int]


   
   Get or set the Include in contact definition only those SURFA nodes/segments within box SABOXID (corresponding to BOXID in *DEFINE_BOX), or if SABOXID is negative, only those SURFA nodes/segments within contact volume |SABOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SABOXID can be used only if SURFATYP is set to 2, 3, or 6, that is, SURFA is a part ID or part set ID. SABOXID is not available for ERODING contact types
















   ..
       !! processed by numpydoc !!

.. py:property:: sbboxid
   :type: Optional[int]


   
   Get or set the Include in contact definition only those SURFB segments within box SBBOXID (corresponding to BOXID in *DEFINE_BOX), or if SBBOXID is negative, only those SURFB segments within contact volume |SBBOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SBBOXID can be used only if SURFBTYP is set to 2, 3, or 6, that is, SURFB is a part ID or part set ID.  SBBOXID is not available for ERODING contact types.
















   ..
       !! processed by numpydoc !!

.. py:property:: sapr
   :type: int


   
   Get or set the Include the SURFA side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
   EQ.0:   Do not include.
   EQ.1 : SURFA side forces included.
   EQ.2 : Same as 1 but also allows for SURFA nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbpr
   :type: int


   
   Get or set the Include the SURFB side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
   EQ.0:   Do not include.
   EQ.1 : SURFB side forces included.
   EQ.2 : Same as 1, but also allows for SURFB nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Static coefficient of friction if FS > 0 and not equal to 2.
   EQ.-1.0: If the frictional coefficients defined in the *PART section are to be used, set FS to a negative number.
   EQ. 2: For contact types SURFACE_TO_SURFACE and ONE_WAY_ SURFACE_TO_SURFACE, the dynamic coefficient of friction points to the table, see DEFINE_TABLE (The table ID is give by FD below.), giving the coefficient of friction as a function of the relative velocity and pressure. This option must be used in combination with the thickness offset option. See Figure 6.1.
   Note: For the special contact option TIED_SURFACE_TO_SURFACE_FAILURE only, the variables FS is the Normal tensile stress at failure.,
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: float


   
   Get or set the Dynamic coefficient of friction. The frictional coefficient is assumed to be dependent on the relative velocity v-rel of the surfaces in contact. Give table ID if FS=2 (default=0.0).
   Note: For the special contact option TIED_SURFACE_TO_SURFACE_ FAILURE only, the variables FD is Shear stress at failure
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Exponential decay coefficient. The frictional coefficient is assumed to be dependent on the relative velocity v-rel of the surfaces in contact. (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: vc
   :type: float


   
   Get or set the Coefficient for viscous friction. This is necessary to limit the friction force to a maximum.
















   ..
       !! processed by numpydoc !!

.. py:property:: vdc
   :type: float


   
   Get or set the Viscous damping coefficient in percent of critical. In order to avoid undesirable oscillation in contact, e.g., for sheet forming simulation, a contact damping perpendicular to the contacting surfaces is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: penchk
   :type: Optional[int]


   
   Get or set the Small penetration in contact search option.  If the tracked node penetrates more than the segment thickness times the factor XPENE (see *CONTROL_CONTACT), the penetration is ignored, and the tracked node is set free.  The thickness is taken as the shell thickness if the segment belongs to a shell element or it is taken as 1/20 of its shortest diagonal if the segment belongs to a solid element.  This option applies to the surface-to-surface contact algorithms.  See Table 0-17 for contact types and more details.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time (contact surface becomes active at this time):LT.0:   Birth time is set to | "BT" | .When negative, birth time is followed during the dynamic relaxation phase of the calculation.After dynamic relaxation has completed, contact is activated regardless of the value of BT.EQ.0 : Birth time is inactive, meaning contact is always activeGT.0 : If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth - time / death - time; see Remark 2 below.Otherwise, if "DT" > 0, birth time applies both duringand after dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time (contact surface is deactivated at this time):LT.0:   If DT = -9999, BT is interpreted as the curve or table ID defining multiple pairs of birth - time / death - time.Otherwise, negative DT indicates that contact is inactive during dynamic relaxation.After dynamic relaxation the birth and death times are followed and set to | "BT" | and | "DT" | , respectively.EQ.0 : DT defaults to 10e20.GT.0 : DT sets the time at which the contact is deactivated.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfsa
   :type: float


   
   Get or set the Scale factor on default SURFA penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.For MORTAR frictional contact this is the stiffness scale factor for the entire contact, and SFSB does not apply.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfsb
   :type: float


   
   Get or set the Scale factor on default SURFA penalty stiffness when SOFT = 0 or SOFT = 2; see also *CONTROL_CONTACT.For MORTAR tied contact, this is an additional stiffness scale factor, resulting in a total stiffness scale of SFSA*SFSB.
















   ..
       !! processed by numpydoc !!

.. py:property:: sast
   :type: Optional[float]


   
   Get or set the Optional thickness for SURFA surface (overrides true thickness). This option applies only to contact with shell elements. SAST has no bearing on the actual thickness of the elements; it only affects the location of the contact surface. For the *CONTACT_TIED_.. options, SAST and SBST below can be defined as negative values, which will cause the determination of whether or not a node is tied to depend only on the separation distance relative to the absolute value of these thicknesses. More information is given under General Remarks on *CONTACT following Optional Card C.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbst
   :type: Optional[float]


   
   Get or set the Optional thickness for SURFA surface (overrides true thickness). This option applies only to contact with shell elements. True thickness is the element thickness of the shell elements. For the TIED options see SAST above.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfsat
   :type: float


   
   Get or set the Scale factor applied to contact thickness of SURFA surface.  This option applies to contact with shell and beam elements.
   SFSAT has no bearing on the actual thickness of the elements; it only affects the location of the contact surface.
   SFSAT is ignored if SAST is nonzero except in the case of MORTAR contact (see Remark 9 in the General Remarks: *Contact section).
















   ..
       !! processed by numpydoc !!

.. py:property:: sfsbt
   :type: float


   
   Get or set the Scale factor applied to contact thickness of SURFA surface.  This option applies only to contact with shell elements.
   SFSAT has no bearing on the actual thickness of the elements; it only affects the location of the contact surface.
   SFSAT is ignored if SBST is nonzero except in the case of MORTAR contact (see Remark 9 in the General Remarks: *Contact section).
















   ..
       !! processed by numpydoc !!

.. py:property:: fsf
   :type: float


   
   Get or set the Coulomb friction scale factor (default=1.0).The Coulomb friction value is scaled as μ_sc=FSF×μ_c; see Mandatory Card 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: vsf
   :type: float


   
   Get or set the Viscous friction scale factor (default=1.0).If this factor is defined, then the limiting force becomes: F_lim =VSF×VC×A_cont ; see Mandatory Card 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: isym
   :type: int


   
   Get or set the Symmetry plane option:
   EQ.0: off,
   EQ.1: do not include faces with normal boundary constraints (e.g., segments of brick elements on a symmetry plane).
   This option is important to retain the correct boundary conditions in the model with symmetry.
















   ..
       !! processed by numpydoc !!

.. py:property:: erosop
   :type: int


   
   Get or set the Erosion/Interior node option:
   EQ.0: only exterior boundary information is saved,
   EQ.1: storage is allocated so that eroding contact can occur. Otherwise, no contact is assumed after erosion of the corresponding element.
















   ..
       !! processed by numpydoc !!

.. py:property:: iadj
   :type: int


   
   Get or set the Adjacent material treatment for solid elements:
   EQ.0: solid element faces are included only for free boundaries,
   EQ.1: solid element faces are included if they are on the boundary of the material subset. This option also allows the erosion within a body and the subsequent treatment of contact.
















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
   :value: 'ERODING_NODES_TO_SURFACE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





