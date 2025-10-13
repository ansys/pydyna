





:class:`SectionBeam`
====================


.. py:class:: section_beam.SectionBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionBeam

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
            - Get or set the Element formulation options:
          * - :py:attr:`~shrf`
            - Get or set the Shear factor. This factor is not needed for truss, resultant beam, discrete beam, and cable elements. The recommended value for rectangular sections is 5/6, the default is 1.0.
          * - :py:attr:`~qr_irid`
            - Get or set the Quadrature rule or rule number for user defined rule for integrated beams:
          * - :py:attr:`~cst`
            - Get or set the Cross section type, not needed for truss, resultant beam, discrete beam, and cable elements:
          * - :py:attr:`~scoor`
            - Get or set the Location of triad for tracking the rotation of the discrete beam element. The force and moment resultants in the output databases are referenced to this triad:
          * - :py:attr:`~nsm`
            - Get or set the Nonstructural mass per unit length.  This option applies to beam types 1-5 and does not apply to discrete, 2D, and spotweld beams, respectively.
          * - :py:attr:`~naupd`
            - Get or set the Neutral axis update option.  See Remark 11.
          * - :py:attr:`~ts1`
            - Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s-direction at node n1. Note that the thickness defined on the *ELEMENT_BEAM_THICKNESS card overrides the definition give here.
          * - :py:attr:`~ts2`
            - Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s-direction at node n2 .
          * - :py:attr:`~tt1`
            - Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t-direction at node n1.
          * - :py:attr:`~tt2`
            - Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t-direction at node n2 .
          * - :py:attr:`~nsloc`
            - Get or set the Location of reference surface normal to s axis for Hughes-Liu beam elements only:
          * - :py:attr:`~ntloc`
            - Get or set the Location of reference surface normal to t axis for Hughes-Liu beam elements only:
          * - :py:attr:`~a`
            - Get or set the Cross-sectional area. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
          * - :py:attr:`~iss`
            - Get or set the Iss . The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
          * - :py:attr:`~itt`
            - Get or set the Itt . The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
          * - :py:attr:`~j`
            - Get or set the J, torsional constant. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here. If J is zero, then J is reset to the sum of ISS+ITT as an approximation.
          * - :py:attr:`~sa`
            - Get or set the Shear area. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
          * - :py:attr:`~ist`
            - Get or set the Ist, product moment of inertia w.r.t. local s- and t-axis. This is only nonzero for unsymmetric cross sections and it can take positive and negative values, e.g. it is negative for SECTION_03.
          * - :py:attr:`~rampt`
            - Get or set the Optional ramp-up time for dynamic relaxation.
          * - :py:attr:`~stress`
            - Get or set the Optional initial stress for dynamic relaxation
          * - :py:attr:`~vol`
            - Get or set the Volume of discrete beam. If the mass density of the material model for the discrete beam is set to unity, the magnitude of the lumped mass can be defined here instead. This lumped mass is partitioned to the two nodes of the beam element. The translational time step size for the type 6 beam is dependent on the volume, mass density, and the translational stiffness values, so it is important to define this parameter. Defining the volume is also essential for mass scaling if the type 6 beam controls the time step size.
          * - :py:attr:`~iner`
            - Get or set the I, lumped inertia of discrete beam which have six degrees of freedom. This lumped inertia is partitioned to the two nodes of the beam element. The rotational time step size for the type 6 beam is dependent on the lumped inertia and the rotational stiffness values, so it is important to define this parameter if the rotational springs are active. Defining the rotational inertia is also essential for mass scaling if the type 6 beam rotational stiffness controls the time step size.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for orientation, materials type ID (66-69, 93 and 95), see *DEFINE_COORDINATE. If CID=0, a default coordinate system is defined in the global system or on the third node of the beam, which is used for orientation. This is not defined for cable elements. The coordinate system rotates with the discrete beam, see SCOOR above.
          * - :py:attr:`~ca`
            - Get or set the Cable area, materials type ID 71, *MAT_CABLE.
          * - :py:attr:`~offset`
            - Get or set the Offset for cable. For a definition see materials type ID 71, *MAT_CABLE.
          * - :py:attr:`~rrcon`
            - Get or set the r-rotational constraint for local coordinate system:
          * - :py:attr:`~srcon`
            - Get or set the s-rotational constraint for local coordinate system:
          * - :py:attr:`~trcon`
            - Get or set the t-rotational constraint for local coordinate system:
          * - :py:attr:`~print`
            - Get or set the Output spot force resultant from spotwelds.
          * - :py:attr:`~itoff`
            - Get or set the Option to specify torsional behavior for spot weld beams.
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

    from section_beam import SectionBeam

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: int


   
   Get or set the Element formulation options:
   EQ.1: Hughes-Liu with cross section integration (default),
   EQ.2: Belytschko-Schwer resultant beam (resultant),
   EQ.3: truss (resultant),
   EQ.4: Belytschko-Schwer full cross-section integration,
   EQ.5: Belytschko-Schwer tubular beam with cross-section integration,
   EQ.6: discrete beam/cable,
   EQ.7: 2D plane strain shell element (xy plane),
   EQ.8: 2D axisymmetric volume weighted shell element (xy plane),
   EQ.9: spotweld beam, see *MAT_SPOTWELD (Type 100).
   Note that the 2D and 3D element types must bot be mixed and different types of 2D elements must not be used together. For example, the plane strain element type must not be used with the axisymmetric element type. In 3D the different beam elements types, i.e., 1-6 and 9 can be freely mixed together.
   EQ.11: Integrated warped beam.
   EQ.12: resultant warped beam
   EQ.13: Small displacement, linear Timoshenko beam with exact stiffness.
   EQ.14: Integrated tubular Elbow element. User defined integration rule with tubular cross section (9) must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: shrf
   :type: float


   
   Get or set the Shear factor. This factor is not needed for truss, resultant beam, discrete beam, and cable elements. The recommended value for rectangular sections is 5/6, the default is 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr_irid
   :type: int


   
   Get or set the Quadrature rule or rule number for user defined rule for integrated beams:
   EQ.1: one integration point,
   EQ.2: 2x2 Gauss quadrature (default beam),
   EQ.3: 3x3 Gauss quadrature,
   EQ.4: 3x3 Lobatto quadrature,
   EQ.5: 4x4 Gauss quadrature,
   EQ.-n: where |n| is the number of the user defined rule. IRID integration rule n is defined using *INTEGRATION_BEAM card.
















   ..
       !! processed by numpydoc !!

.. py:property:: cst
   :type: int


   
   Get or set the Cross section type, not needed for truss, resultant beam, discrete beam, and cable elements:
   EQ.0: rectangular (default),
   EQ.1: tubular,
   EQ.2: arbitrary (user defined integration rule).
















   ..
       !! processed by numpydoc !!

.. py:property:: scoor
   :type: float


   
   Get or set the Location of triad for tracking the rotation of the discrete beam element. The force and moment resultants in the output databases are referenced to this triad:
   EQ.-1.0: beam node 1, the angular velocity of node 1 rotates triad,
   EQ. 0.0: centered between beam nodes 1 and 2, the average angular velocity of nodes 1 and 2 is used to rotate the triad (default),
   EQ.+1.0: beam node 2, the angular velocity of node 2 rotates triad.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsm
   :type: float


   
   Get or set the Nonstructural mass per unit length.  This option applies to beam types 1-5 and does not apply to discrete, 2D, and spotweld beams, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: naupd
   :type: int


   
   Get or set the Neutral axis update option.  See Remark 11.
   EQ. 0:  Not used
   EQ.1.0:         Update the neutral axis when damage or failure occurs at  one or more integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: ts1
   :type: Optional[float]


   
   Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s-direction at node n1. Note that the thickness defined on the *ELEMENT_BEAM_THICKNESS card overrides the definition give here.
















   ..
       !! processed by numpydoc !!

.. py:property:: ts2
   :type: Optional[float]


   
   Get or set the Beam thickness (CST=0.0, 2.0) or outer diameter (CST = 1.0) in s-direction at node n2 .
















   ..
       !! processed by numpydoc !!

.. py:property:: tt1
   :type: Optional[float]


   
   Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t-direction at node n1.
















   ..
       !! processed by numpydoc !!

.. py:property:: tt2
   :type: Optional[float]


   
   Get or set the Beam thickness (CST=0.0, 2.0) or inner diameter (CST = 1.0) in t-direction at node n2 .
















   ..
       !! processed by numpydoc !!

.. py:property:: nsloc
   :type: Optional[float]


   
   Get or set the Location of reference surface normal to s axis for Hughes-Liu beam elements only:
   EQ.1.0: side at s=1,
   EQ.0.0: center (default),
   EQ.-1.0: side at s=-1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ntloc
   :type: Optional[float]


   
   Get or set the Location of reference surface normal to t axis for Hughes-Liu beam elements only:
   EQ.1.0: side at t=,
   EQ.0.0: center (default),
   EQ.-1: side at t=-1.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Cross-sectional area. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
















   ..
       !! processed by numpydoc !!

.. py:property:: iss
   :type: Optional[float]


   
   Get or set the Iss . The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
















   ..
       !! processed by numpydoc !!

.. py:property:: itt
   :type: Optional[float]


   
   Get or set the Itt . The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
















   ..
       !! processed by numpydoc !!

.. py:property:: j
   :type: Optional[float]


   
   Get or set the J, torsional constant. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here. If J is zero, then J is reset to the sum of ISS+ITT as an approximation.
















   ..
       !! processed by numpydoc !!

.. py:property:: sa
   :type: Optional[float]


   
   Get or set the Shear area. The definition on *ELEMENT_BEAM_THICKNESS overrides the value defined here.
















   ..
       !! processed by numpydoc !!

.. py:property:: ist
   :type: Optional[float]


   
   Get or set the Ist, product moment of inertia w.r.t. local s- and t-axis. This is only nonzero for unsymmetric cross sections and it can take positive and negative values, e.g. it is negative for SECTION_03.
















   ..
       !! processed by numpydoc !!

.. py:property:: rampt
   :type: Optional[float]


   
   Get or set the Optional ramp-up time for dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: stress
   :type: Optional[float]


   
   Get or set the Optional initial stress for dynamic relaxation
















   ..
       !! processed by numpydoc !!

.. py:property:: vol
   :type: Optional[float]


   
   Get or set the Volume of discrete beam. If the mass density of the material model for the discrete beam is set to unity, the magnitude of the lumped mass can be defined here instead. This lumped mass is partitioned to the two nodes of the beam element. The translational time step size for the type 6 beam is dependent on the volume, mass density, and the translational stiffness values, so it is important to define this parameter. Defining the volume is also essential for mass scaling if the type 6 beam controls the time step size.
















   ..
       !! processed by numpydoc !!

.. py:property:: iner
   :type: Optional[float]


   
   Get or set the I, lumped inertia of discrete beam which have six degrees of freedom. This lumped inertia is partitioned to the two nodes of the beam element. The rotational time step size for the type 6 beam is dependent on the lumped inertia and the rotational stiffness values, so it is important to define this parameter if the rotational springs are active. Defining the rotational inertia is also essential for mass scaling if the type 6 beam rotational stiffness controls the time step size.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for orientation, materials type ID (66-69, 93 and 95), see *DEFINE_COORDINATE. If CID=0, a default coordinate system is defined in the global system or on the third node of the beam, which is used for orientation. This is not defined for cable elements. The coordinate system rotates with the discrete beam, see SCOOR above.
















   ..
       !! processed by numpydoc !!

.. py:property:: ca
   :type: Optional[float]


   
   Get or set the Cable area, materials type ID 71, *MAT_CABLE.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: Optional[float]


   
   Get or set the Offset for cable. For a definition see materials type ID 71, *MAT_CABLE.
















   ..
       !! processed by numpydoc !!

.. py:property:: rrcon
   :type: float


   
   Get or set the r-rotational constraint for local coordinate system:
   EQ.0.0: Coordinate ID rotates about r axis with nodes (default),
   EQ.1.0: Rotation is constrained about the r-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: srcon
   :type: float


   
   Get or set the s-rotational constraint for local coordinate system:
   EQ.0.0: Coordinate ID rotates about s axis with nodes (default),
   EQ.1.0: Rotation is constrained about the s-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: trcon
   :type: float


   
   Get or set the t-rotational constraint for local coordinate system:
   EQ.0.0: Coordinate ID rotates about t axis with nodes (default),
   EQ.1.0: Rotation is constrained about the t-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: print
   :type: Optional[float]


   
   Get or set the Output spot force resultant from spotwelds.
   EQ.0.0: Data is output to SWFORC file.
   EQ.1.0: Output is surpressed.
















   ..
       !! processed by numpydoc !!

.. py:property:: itoff
   :type: Optional[float]


   
   Get or set the Option to specify torsional behavior for spot weld beams.
   EQ.0.0: Torsional stiffness is active.
   EQ.1.0 : Torsional stiffness is zero(free to twist).
















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
   :value: 'BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





