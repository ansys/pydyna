





:class:`SectionShell`
=====================


.. py:class:: section_shell.SectionShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~elform`
            - Get or set the ELFORM Element formulation options, see Remarks 1 and 3:
          * - :py:attr:`~shrf`
            - Get or set the Shear factor which scales the transverse shear stress (default =1.0).
          * - :py:attr:`~nip`
            - Get or set the Number of through shell thickness integration points. Default is set to 2.
          * - :py:attr:`~propt`
            - Get or set the Printout option:
          * - :py:attr:`~qr_irid`
            - Get or set the Quadrature rule or Integration rule ID, see *INTEGRATION_SHELL:
          * - :py:attr:`~icomp`
            - Get or set the Flag for orthotropic/anisotropic layered composite material model. This option applies to material types 22, 23, 33, 34, 36, 40, 41-50, 54-56, 58, 59, 103, 116 and 194:
          * - :py:attr:`~setyp`
            - Get or set the 2D solid element type: Defined for ELFORM 13, 14, and 15:
          * - :py:attr:`~t1`
            - Get or set the Shell thickness at node n1 , unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
          * - :py:attr:`~t2`
            - Get or set the Shell thickness at node n2, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
          * - :py:attr:`~t3`
            - Get or set the Shell thickness at node n3, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
          * - :py:attr:`~t4`
            - Get or set the Shell thickness at node n4, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
          * - :py:attr:`~nloc`
            - Get or set the Location of reference surface for three dimensional shell elements. If nonzero, the offset distance from the plane of the nodal points to the reference surface of the shell in the direction of the shell normal vector is a value offset = -0.50*NLOC*(average shell thickness). This offset is not considered in the contact subroutines unless CNTCO is set to 1 in *CONTROL_SHELL. Alternatively, the offset can be specified by using the OFFSET option in the *ELEMENT_SHELL input section.
          * - :py:attr:`~marea`
            - Get or set the Non-structural mass per unit area.  This is additional mass which comes from materials, such as carpeting.
          * - :py:attr:`~idof`
            - Get or set the Treatment of through thickness strain (see Remark 7).
          * - :py:attr:`~edgset`
            - Get or set the Edge node set required for shell type seatbelts.  See Remark 8.
          * - :py:attr:`~angle`
            - dynamic array of beta-i: material angle at ith-integration point..
          * - :py:attr:`~nipp`
            - Get or set the Number of in-plane integration points for user-defined shell (0 if resultant/discrete element).
          * - :py:attr:`~nxdof`
            - Get or set the Number of extra degrees of freedom per node for user-defined shell.
          * - :py:attr:`~iunf`
            - Get or set the Flag for using nodal fiber vectors in user-defined shell.
          * - :py:attr:`~ihgf`
            - Get or set the Flag for using hourglass stabilization (NIPP.GT.0).
          * - :py:attr:`~itaj`
            - Get or set the Flag for setting up finite element matrices (NIPP.GT.0).
          * - :py:attr:`~lmc`
            - Get or set the Number of property parameters.
          * - :py:attr:`~nhsv`
            - Get or set the Number of history variables.
          * - :py:attr:`~iloc`
            - Get or set the Coordinate system option.
          * - :py:attr:`~integration_points`
            - Get the table of integration_points.
          * - :py:attr:`~pi`
            - dynamic array of LMC property parameters..
          * - :py:attr:`~title`
            - Get or set the Additional title line


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

    from section_shell import SectionShell

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the ELFORM Element formulation options, see Remarks 1 and 3:
   EQ.1: Hughes-Liu,
   EQ.2: Belytschko-Tsay (default),
   EQ.3: BCIZ triangular shell,
   EQ.4: C0 triangular shell,
   EQ.5: Belytschko-Tsay membrane,
   EQ.6: S/R Hughes-Liu,
   EQ.7: S/R co-rotational Hughes-Liu,
   EQ.8: Belytschko-Leviathan shell,
   EQ.9: Fully integrated Belytschko-Tsay membrane,
   EQ.10: Belytschko-Wong-Chiang,
   EQ.11: Fast (co-rotational) Hughes-Liu,
   EQ.12: Plane stress (x-y plane),
   EQ.13: Plane strain (x-y plane),
   EQ.14: Axisymmetric solid (xy-plane, y-axis of symmetry) - area weighted (see Remark 11),
   EQ.15: Axisymmetric solid (y-axis of symmetry) - volume weighted,
   EQ.16: Fully integrated shell element (very fast),
   EQ.-16: Fully integrated shell element modified for higher accuracy,
   EQ.17 Fully integrated DKT, triangular shell element.  See Remark 10,
   EQ.18: Fully Integrated linear DK qaudrilateral/triangular shell, See Remarks 2 and 3.
   EQ.20: Fully integrated linear assumed strain C0 shell, See Remark 3.
   EQ.21: Fully integrated linear strain C0 shell (5DOF)
   EQ.22: Linear shear panel element (3 DOF per node), See Remark 4.
   EQ.23: 8-node quadrilateral shell
   EQ.24: 6-node quadratic triangular shell
   EQ.25: Belytschko-Tsay shell with thickness stretch.
   EQ.26: Fully integrated shell with thickness stretch.
   EQ.27: C0 triangular shell with thickness stretch.
   EQ.29: Cohesive shell element for edge-to-edge connection of shells.  See Remark 13.
   EQ.-29:Cohesive shell element for edge-to-edge connection of shells (more suitable for pure shear).  See Remark 13.
   EQ.30:  Fast fully integrated element with 2 in-plane integration points based on ELFORM 16
   EQ.31: 1 point eulerian Navier-Stokes,
   EQ.32: 8 point Eulerian Navier-Stokes,
   EQ.33: CVFEM Eulerian Navier-Stokes.EQ.
   EQ.41: Mesh-free (EFG) shell local approach. (more suitable for crashworthiness analysis)
   EQ.42: Mesh-free (EFG) shell global approach. (more suitable for metal forming analysis)
   EQ.43: Mesh-free (EFG) plane strain formulation (x-y plane).
   EQ.44: Mesh-free (EFG) axisymmetric solid formulation (x-y plane, y-axis of symmetry).
   46: Cohesive element for two-dimensional plane strain, plane stress, and area-weighted axisymmetric problems (type 14 shells).
   EQ.47: Cohesive element for two-dimensional volume-weighted axisymmetric problems (use with type 15 shells).
   EQ.52:  Plane strain (xy-plane) XFEM, base element type 13 with full integration. See Remark 9.EQ.54:   Shell XFEM, base element type defined by BASELM(default 2).See Remark 9.
   EQ.55 : 8 - node singular plane strain(xy - plane) finite element.See Remark 12.
   EQ.98 : Interpolation shell
   EQ.99 : Simplified linear element for time - domain vibration studies.See Remark 5.
   EQ.101 : User defined shell
   EQ.102 : User defined shell
   EQ.103 : User defined shell
   EQ.104 : User defined shell
   EQ.105 : User defined shell
   EQ.201 : Isogeometric shells with NURBS.See * ELEMENT_SHELL_NURBS_PATCH.
   GE.1000 : Generalized shell element formulation(user defined).See * DEFINE_ELEMENT_GENERALIZED_SHELL.
   Note that the 2D and 3D element types must not be mixed,and different types of 2D elements must not be used together.For example,
   two - dimensional axisymmetric calculations can use either element types 14 or 15, but these element types must not be mixed together.Likewise,
   the plane strain element type must not be used with either the plane stress element or the axisymmetric element types.
   In three dimensions, the different shell elements types, i.e., 1 - 11 and 16, can be freely mixed together.
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: float


   
   Get or set the Shear factor which scales the transverse shear stress (default =1.0).
   A suggested value is 5/6.
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: int


   
   Get or set the Number of through shell thickness integration points. Default is set to 2.
   Through thickness integration for the two-dimensional elements (options 11-15 above) is not meaningful; consequently, the default is equal to 1 integration point.  Fully integrated two-dimensional elements are available for options 13 and 15 by setting NIP equal to a value of 4 corresponding to a 2x2 Gaussian quadrature.
















   ..
       !! processed by numpydoc !!

.. py:property:: propt
   :type: float


   
   Get or set the Printout option:
   EQ.1: average resultants and fiber lengths (default),
   EQ.2: resultants at plan points and fiber lengths,
   EQ.3: resultants, stresses at all points, fiber lengths.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr_irid
   :type: int


   
   Get or set the Quadrature rule or Integration rule ID, see *INTEGRATION_SHELL:
   LT.0: absolute value is specified rule number,
   EQ.0: Gauss (up to ten points are permitted),
   EQ.1: trapezoidal, not recommend for accuracy reasons.
















   ..
       !! processed by numpydoc !!

.. py:property:: icomp
   :type: int


   
   Get or set the Flag for orthotropic/anisotropic layered composite material model. This option applies to material types 22, 23, 33, 34, 36, 40, 41-50, 54-56, 58, 59, 103, 116 and 194:
   EQ.0: Flag is tuned off (default),
   EQ.1: a material angle in degrees is defined for each through thickness integration point. Thus, each layer has one integration point.
















   ..
       !! processed by numpydoc !!

.. py:property:: setyp
   :type: int


   
   Get or set the 2D solid element type: Defined for ELFORM 13, 14, and 15:
   EQ.1: Lagrangian,
   EQ.2: Eulerian (single material with voids),
   EQ.3: ALE
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: float


   
   Get or set the Shell thickness at node n1 , unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: float


   
   Get or set the Shell thickness at node n2, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
















   ..
       !! processed by numpydoc !!

.. py:property:: t3
   :type: float


   
   Get or set the Shell thickness at node n3, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
















   ..
       !! processed by numpydoc !!

.. py:property:: t4
   :type: float


   
   Get or set the Shell thickness at node n4, unless the thickness is defined on the *ELEMENT_SHELL_OPTION card.
















   ..
       !! processed by numpydoc !!

.. py:property:: nloc
   :type: float


   
   Get or set the Location of reference surface for three dimensional shell elements. If nonzero, the offset distance from the plane of the nodal points to the reference surface of the shell in the direction of the shell normal vector is a value offset = -0.50*NLOC*(average shell thickness). This offset is not considered in the contact subroutines unless CNTCO is set to 1 in *CONTROL_SHELL. Alternatively, the offset can be specified by using the OFFSET option in the *ELEMENT_SHELL input section.
   EQ.1.0: top surface,
   EQ.0.0: mid-surface (default),
   EQ.-1.0: bottom surface.For nonzero offset distances, the time step size is reduced to prevent instabilities. See NLOCDT in *CONTROL_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: marea
   :type: float


   
   Get or set the Non-structural mass per unit area.  This is additional mass which comes from materials, such as carpeting.
   This mass is not directly included in the time step calculation.
   Another and often more convenient alternative for defining distributed mass is with *ELEMENT_MASS_PART,
   which allows additional non-structural mass to be distributed by an area weighted distribution to all nodes of a given part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: idof
   :type: float


   
   Get or set the Treatment of through thickness strain (see Remark 7).
   LT.0:   Same as IDOF = 3 but the contact pressure is averaged over a time - IDOF in order to reduce noise and thus improve stability.
   EQ.1: The thickness field is continous across the element edges for metalforming applications. This is the default.
   EQ.2: The thickness field is discontinous across the element edges. This is necessary for crashworthiness applications due to shell intersections, sharp included angles, and non-smooth deformations.This option applies to element types 25, 26 and 27. This is the default for these element types.
   EQ.3:The thickness strain is governed by the stress from contact and pressure loads, meaning that the strain is adjusted for the through thickness stress to equilibrate the contact and load pressure. Please note that it is assumed that the pressure is positive, i.e., acting towards the shell surface. Thus, vacuum loads cannot be used with this option. This option applies to element types 2, 4, and ±16
   EQ.11 : Same as IDOF = 1 but the through thickness strain is simplified in the kinematics in order to reduce locking effects which were observed for considerable thickness changes.This option applies to element type 25 only.
   EQ.12 : Same as IDOF = 2 but the through thickness strain is simplified in the kinematics in order to reduce locking effects which were observed for considerable thickness changes.This option applies to element type 25 only.
















   ..
       !! processed by numpydoc !!

.. py:property:: edgset
   :type: int


   
   Get or set the Edge node set required for shell type seatbelts.  See Remark 8.
   See Figure Error! Reference source not found. in *ELEMENT_SEATBELT for additional clarification.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of beta-i: material angle at ith-integration point..
















   ..
       !! processed by numpydoc !!

.. py:property:: nipp
   :type: int


   
   Get or set the Number of in-plane integration points for user-defined shell (0 if resultant/discrete element).
















   ..
       !! processed by numpydoc !!

.. py:property:: nxdof
   :type: int


   
   Get or set the Number of extra degrees of freedom per node for user-defined shell.
















   ..
       !! processed by numpydoc !!

.. py:property:: iunf
   :type: int


   
   Get or set the Flag for using nodal fiber vectors in user-defined shell.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihgf
   :type: int


   
   Get or set the Flag for using hourglass stabilization (NIPP.GT.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: itaj
   :type: int


   
   Get or set the Flag for setting up finite element matrices (NIPP.GT.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: lmc
   :type: int


   
   Get or set the Number of property parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhsv
   :type: int


   
   Get or set the Number of history variables.
















   ..
       !! processed by numpydoc !!

.. py:property:: iloc
   :type: int


   
   Get or set the Coordinate system option.
















   ..
       !! processed by numpydoc !!

.. py:property:: integration_points
   :type: pandas.DataFrame


   
   Get the table of integration_points.
















   ..
       !! processed by numpydoc !!

.. py:property:: pi
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of LMC property parameters..
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'SECTION'


.. py:attribute:: subkeyword
   :value: 'SHELL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





