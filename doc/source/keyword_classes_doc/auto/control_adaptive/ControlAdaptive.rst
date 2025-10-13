





:class:`ControlAdaptive`
========================


.. py:class:: control_adaptive.ControlAdaptive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ADAPTIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAdaptive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~adpfreq`
            - Get or set the Time interval between adaptive refinements.
          * - :py:attr:`~adptol`
            - Get or set the Adaptive error tolerance in degrees for ADPOPT set to 1 or 2 below. If ADPOPT is set to 8, ADPTOL is the characteristic element size (default = 1.0E+20).
          * - :py:attr:`~adptyp`
            - Get or set the Adaptive options:
          * - :py:attr:`~maxlvl`
            - Get or set the Maximum number of refinement levels (default = 3).
          * - :py:attr:`~tbirth`
            - Get or set the Birth time at which the adaptive remeshing begins (default = 0.0).
          * - :py:attr:`~tdeath`
            - Get or set the Death time at which the adaptive remeshing ends (default = 1.0E+20).
          * - :py:attr:`~lcadp`
            - Get or set the Adaptive interval is changed as a function of time given by load curve ID, LCADP.
          * - :py:attr:`~ioflag`
            - Get or set the Flag to generate adaptive mesh at exit including *NODE, *ELEMENT, *SHELL, *BOUNDARY_, *CONTACT_NODE_, and *CONSTRAINED_ ADAPTIVITY to be saved in the file, adapt.msh.
          * - :py:attr:`~adpsize`
            - Get or set the Minimum element size to be adapted based on element edge length. If undefined the edge length limit is ignored (default = 0.0).
          * - :py:attr:`~adpass`
            - Get or set the One or two pass adaptivity flag:
          * - :py:attr:`~ireflg`
            - Get or set the Uniform refinement level. A values of 1, 2, 3, ... allow 4, 16, 64, ....  elements, respectively, to be created uniformly for each original element.
          * - :py:attr:`~adpene`
            - Get or set the Adapt the mesh when the contact surfaces approach or penetrate the tooling surface.
          * - :py:attr:`~adpth`
            - Get or set the Absolute shell thickness level below which adaptive remeshing should begin.
          * - :py:attr:`~memory`
            - Get or set the See keyword manual.
          * - :py:attr:`~orient`
            - Get or set the This option applies to the FORMING contact option only.
          * - :py:attr:`~maxel`
            - Get or set the Adaptivity is stopped if this number of elements is exceeded
          * - :py:attr:`~ladpn90`
            - Get or set the Maximum number of elements covering 90 degree of radii.
          * - :py:attr:`~ladpgh`
            - Get or set the Fission flag for neighbor splitting
          * - :py:attr:`~ncfred`
            - Get or set the Frequency of fission to fusion steps.  For example, if NCFREQ=4, then fusion will occur on the fourth, eighth, twelfth,  etc., fission steps, respectively.  If this option is used NCFREQ>1 is recommended
          * - :py:attr:`~ladpcl`
            - Get or set the Fusion will not occur until the fission level reaches IADPCL.  Therefore, if IADPCL=2, MAXLVL=5,  any  element can be split into 256 elements.  If the surface flattens out, the number of elements will be reduced if the fusion option is active, i.e.,  the 256 elements can be fused and reduced to 16
          * - :py:attr:`~adpctl`
            - Get or set the Adaptivity error tolerance in degrees for activating fusion.  It follows the same rules as ADPOPT above
          * - :py:attr:`~cbirth`
            - Get or set the Birth time for adaptive fusion.  If ADPENE>0, look-ahead adaptivity is active.  In this case, fission, based on local tool curvature, will occur while the blank is still relatively flat.  The time value given for CBIRTH should be set to a time later in the simulation after the forming process is well underway.
          * - :py:attr:`~cdeath`
            - Get or set the Death time for adaptive fusion
          * - :py:attr:`~lclvl`
            - Get or set the Load curve ID of a curve that defines the maximum refinement level as a function of time
          * - :py:attr:`~cnla`
            - Get or set the Limit angle for corner nodes
          * - :py:attr:`~mmm2d`
            - Get or set the If non-zero, common boundaries of all adapted materials will be merged. Only for 2D r-adaptivity
          * - :py:attr:`~adperr`
            - Get or set the 3-digit number, as "XYY", where "X" and "YY" define the options for the recovery techniques and the error estimators, respectively.
          * - :py:attr:`~d3trace`
            - Get or set the Flag that is either 0 or 1. If set to 1 then a d3plot state will be output
          * - :py:attr:`~iadpcf`
            - Get or set the Flag to enable adaptive user control files:
          * - :py:attr:`~ifsand`
            - Get or set the Set this flag to “1” for sandwiched sheet forming


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

    from control_adaptive import ControlAdaptive

Property detail
---------------

.. py:property:: adpfreq
   :type: Optional[float]


   
   Get or set the Time interval between adaptive refinements.
















   ..
       !! processed by numpydoc !!

.. py:property:: adptol
   :type: float


   
   Get or set the Adaptive error tolerance in degrees for ADPOPT set to 1 or 2 below. If ADPOPT is set to 8, ADPTOL is the characteristic element size (default = 1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: adptyp
   :type: int


   
   Get or set the Adaptive options:
   EQ.1: angle change in degrees per adaptive refinement relative to the surrounding elements for each element to be refined (default).
   EQ.2: total angle change in degrees relative to the surrounding element for each element to be refined.
   Adapts when the shell error in the energy norm, Δe, exceeds ADPTOL/100 times the mean energy norm within the part.
   EQ.7: 3D r-adaptive remeshing for solid elements.  Tetrahedrons are used in the adaptive remeshing process (solid formulation 10 or 13, or if EFG, formulation 42), or in the case of 3D axisymmetry (orbital) adaptivity, hexahedral and pentahedral elements are used in the adaptive remeshing.  A completely new mesh is generated which is initialized from the old mesh using a least squares approximation.  The mesh size is currently based on the minimum and maximum edge lengths defined on the *CONTROL_REMESHING keyword input.  This option remains under development, and we are not sure of its reliability on complex geometries.
   EQ.8/-8: 2D r-adaptive remeshing for plane stress, plane strain, and axisymmetric continuum elements,that is, shell formulations 12 through 15.
   A completely new mesh is generated which is initialized from the old mesh using a least squares approximation.
   The mesh size is currently based on the value, ADPTOL, which gives the characteristic element size.
   This option is based on earlier work by Dick and Harris[1992].
   If ADPTYP is negative, then self-contacting material will not be merged together.
   The self-merging is often preferred since it eliminates sharp folds in the boundary;
   however, if the sharp fold is being simulated, unexpected results are generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxlvl
   :type: int


   
   Get or set the Maximum number of refinement levels (default = 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Birth time at which the adaptive remeshing begins (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time at which the adaptive remeshing ends (default = 1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcadp
   :type: int


   
   Get or set the Adaptive interval is changed as a function of time given by load curve ID, LCADP.
   EQ.0: ADPFREQ is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioflag
   :type: int


   
   Get or set the Flag to generate adaptive mesh at exit including *NODE, *ELEMENT, *SHELL, *BOUNDARY_, *CONTACT_NODE_, and *CONSTRAINED_ ADAPTIVITY to be saved in the file, adapt.msh.
   EQ.0: no adaptive mesh generation at the exit,
   EQ.1: adaptive mesh generation at the exit.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpsize
   :type: float


   
   Get or set the Minimum element size to be adapted based on element edge length. If undefined the edge length limit is ignored (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: adpass
   :type: int


   
   Get or set the One or two pass adaptivity flag:
   EQ.0: two pass adaptivity,
   EQ.1: one pass adaptivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: ireflg
   :type: int


   
   Get or set the Uniform refinement level. A values of 1, 2, 3, ... allow 4, 16, 64, ....  elements, respectively, to be created uniformly for each original element.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpene
   :type: float


   
   Get or set the Adapt the mesh when the contact surfaces approach or penetrate the tooling surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: adpth
   :type: float


   
   Get or set the Absolute shell thickness level below which adaptive remeshing should begin.
   EQ.0: ADPTH is ignored (default).
   This option works only if ADPTOL is nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: memory
   :type: int


   
   Get or set the See keyword manual.
   EQ.0: MEMORY is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: orient
   :type: int


   
   Get or set the This option applies to the FORMING contact option only.
   EQ.0: LS-DYNA sets the global orientation of the contact surface the first time a potential contact is observed after the birth time,
   EQ.1: the user orientation for the contact interface is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxel
   :type: int


   
   Get or set the Adaptivity is stopped if this number of elements is exceeded
   EQ.0: MAXEL is ignored (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: ladpn90
   :type: int


   
   Get or set the Maximum number of elements covering 90 degree of radii.
















   ..
       !! processed by numpydoc !!

.. py:property:: ladpgh
   :type: int


   
   Get or set the Fission flag for neighbor splitting
   EQ:0 split all neighbor elements
   EQ:1 do not split neighbor elements
















   ..
       !! processed by numpydoc !!

.. py:property:: ncfred
   :type: Optional[int]


   
   Get or set the Frequency of fission to fusion steps.  For example, if NCFREQ=4, then fusion will occur on the fourth, eighth, twelfth,  etc., fission steps, respectively.  If this option is used NCFREQ>1 is recommended
















   ..
       !! processed by numpydoc !!

.. py:property:: ladpcl
   :type: int


   
   Get or set the Fusion will not occur until the fission level reaches IADPCL.  Therefore, if IADPCL=2, MAXLVL=5,  any  element can be split into 256 elements.  If the surface flattens out, the number of elements will be reduced if the fusion option is active, i.e.,  the 256 elements can be fused and reduced to 16
















   ..
       !! processed by numpydoc !!

.. py:property:: adpctl
   :type: Optional[float]


   
   Get or set the Adaptivity error tolerance in degrees for activating fusion.  It follows the same rules as ADPOPT above
















   ..
       !! processed by numpydoc !!

.. py:property:: cbirth
   :type: float


   
   Get or set the Birth time for adaptive fusion.  If ADPENE>0, look-ahead adaptivity is active.  In this case, fission, based on local tool curvature, will occur while the blank is still relatively flat.  The time value given for CBIRTH should be set to a time later in the simulation after the forming process is well underway.
















   ..
       !! processed by numpydoc !!

.. py:property:: cdeath
   :type: float


   
   Get or set the Death time for adaptive fusion
















   ..
       !! processed by numpydoc !!

.. py:property:: lclvl
   :type: Optional[int]


   
   Get or set the Load curve ID of a curve that defines the maximum refinement level as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: cnla
   :type: float


   
   Get or set the Limit angle for corner nodes
   GT.0.0: Limit angle is CNLA and simplified boundary lines for straight sections are used as remeshing basis.
   LT.0.0: Limit angle is |CNLA| and accurate boundary lines are used as remeshing basis (recommended).
















   ..
       !! processed by numpydoc !!

.. py:property:: mmm2d
   :type: int


   
   Get or set the If non-zero, common boundaries of all adapted materials will be merged. Only for 2D r-adaptivity
















   ..
       !! processed by numpydoc !!

.. py:property:: adperr
   :type: str


   
   Get or set the 3-digit number, as "XYY", where "X" and "YY" define the options for the recovery techniques and the error estimators, respectively.
   For X:
   EQ.0: superconvergent patch recovery (SPR) (default);
   EQ.1: the least square fit of the stress to the nodes (Global L2);
   EQ.2: error density SPR;
   EQ.3: self-weighted SPR
   For YY:
   EQ.00: energy norm (default)
   EQ.01: Cauchy sigma_x
   EQ.02: sigma_y
   EQ.03: sigma_z
   EQ.04: tau_xy
   EQ.05: tau_yz
   EQ.06: tau_zx
   EQ.07: effective plastic strain, eps_ep
   EQ.08: pressure
   EQ.09: von Mises
   EQ.10: principal deviator stress s11
   EQ.11: S22
   EQ.12: S33
   EQ.13: Tresca
   EQ.14: principal stress sigma_11
   EQ.15: sigma_22
   EQ.16: sigma_33
   EQ.20: user subroutine "uadpval" to extract the numerical solutions for recovery, and "uadpnorm" to provide an error estimator
















   ..
       !! processed by numpydoc !!

.. py:property:: d3trace
   :type: int


   
   Get or set the Flag that is either 0 or 1. If set to 1 then a d3plot state will be output
   just before and after an adaptive step even though it may not be
   requested. The reason for wanting to do this is to allow the LS-PrePost particle trace algorithm to work in the case of adaptivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: iadpcf
   :type: int


   
   Get or set the Flag to enable adaptive user control files:
   EQ.0:   No user control files
   EQ.1 : Perform run - time control on 3D adaptivity through control files
   See details of this option in Manual Volume IV : Multiscale Solvers
















   ..
       !! processed by numpydoc !!

.. py:property:: ifsand
   :type: int


   
   Get or set the Set this flag to “1” for sandwiched sheet forming
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ADAPTIVE'






