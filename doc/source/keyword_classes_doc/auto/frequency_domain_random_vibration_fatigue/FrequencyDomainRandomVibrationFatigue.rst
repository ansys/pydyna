





:class:`FrequencyDomainRandomVibrationFatigue`
==============================================


.. py:class:: frequency_domain_random_vibration_fatigue.FrequencyDomainRandomVibrationFatigue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_RANDOM_VIBRATION_FATIGUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainRandomVibrationFatigue

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mdmin`
            - Get or set the The first mode in modal superposition method (optional).
          * - :py:attr:`~mdmax`
            - Get or set the The last mode in modal superposition method (optional).
          * - :py:attr:`~fnmin`
            - Get or set the The minimum natural frequency in modal superposition method(optional).
          * - :py:attr:`~fnmax`
            - Get or set the The maximum natural frequency in modal superposition method(optional).
          * - :py:attr:`~restrt`
            - Get or set the Restart option.
          * - :py:attr:`~dampf`
            - Get or set the Modal damping coefficient, ζ.
          * - :py:attr:`~lcdam`
            - Get or set the Load Curve ID defining mode dependent modal damping coefficient, ζ.
          * - :py:attr:`~lctyp`
            - Get or set the Type of load curve defining modal damping coefficient
          * - :py:attr:`~dmpmas`
            - Get or set the Mass proportional damping constant α, in Rayleigh damping.
          * - :py:attr:`~dmpstf`
            - Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
          * - :py:attr:`~dmptyp`
            - Get or set the Type of damping
          * - :py:attr:`~vaflag`
            - Get or set the Loading type:
          * - :py:attr:`~method`
            - Get or set the method for modal response analysis.
          * - :py:attr:`~unit`
            - Get or set the Flag for acceleration unit conversion:
          * - :py:attr:`~umlt`
            - Get or set the Multiplier for converting g to [length unit]/[time unit]2(used only for UNIT = -1).
          * - :py:attr:`~vapsd`
            - Get or set the Flag for PSD output:
          * - :py:attr:`~varms`
            - Get or set the Flag for RMS output:
          * - :py:attr:`~napsd`
            - Get or set the Number of auto PSD load definition. Card 5 is repeated "NAPSD" number of times,one for each auto PSD load definition. The default value is 1.
          * - :py:attr:`~ncpsd`
            - Get or set the Number of cross PSD load definition. Card 6 is repeated "NCPSD" times,one for each cross PSD load definition. The default value is 0.
          * - :py:attr:`~ldtyp`
            - Get or set the Excitation load (LDPSD in card 5) type:
          * - :py:attr:`~ipanelu`
            - Get or set the Number of strips in U direction (used only for VAFLAG=5, 6, 7).
          * - :py:attr:`~ipanelv`
            - Get or set the Number of strips in V direction (used only for VAFLAG=5, 6, 7).
          * - :py:attr:`~temper`
            - Get or set the Temperature.
          * - :py:attr:`~ldflag`
            - Get or set the Type of loading curves.
          * - :py:attr:`~icoarse`
            - Get or set the Option for PSD curve coarsening:
          * - :py:attr:`~tcoarse`
            - Get or set the Tolerance for slope change percentage for removing intermediate points from PSD curve (default is 0.1), for ICOARSE  =  2.
          * - :py:attr:`~sid`
            - Get or set the GE.0: Set ID for the panel exposed to acoustic environment,or the nodes subjected to nodal force excitation,or nodal acceleration excitation.For VAFLAG = 1,base acceleration,leave this as blank
          * - :py:attr:`~stype`
            - Get or set the Flag specifying meaning of SID.
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom for nodal force excitation or base acceleration(DOF = 1, 2 and 3),or wave direction:
          * - :py:attr:`~ldpsd`
            - Get or set the Load curve for PSD, SPL or time history excitation.
          * - :py:attr:`~ldvel`
            - Get or set the Load curve for phase velocity.
          * - :py:attr:`~ldflw`
            - Get or set the Load curve for exponential decay for TBL in flow-wise direction.
          * - :py:attr:`~ldspn`
            - Get or set the Load curve for exponential decay for TBL in span-wise direction.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for defining wave direction, see
          * - :py:attr:`~load_i`
            - Get or set the ID of load i for cross PSD.
          * - :py:attr:`~load_j`
            - Get or set the ID of load j for cross PSD.
          * - :py:attr:`~lctyp2`
            - Get or set the Type of load curves (LDPSD1 and LDPSD2) for defining cross PSD:
          * - :py:attr:`~ldpsd1`
            - Get or set the Load curve for real part or magnitude of cross PSD.
          * - :py:attr:`~ldpsd2`
            - Get or set the Load curve for imaginary part or phase angle of cross PSD.
          * - :py:attr:`~mftg`
            - Get or set the Method for random fatigue analysis (for option _FATIGUE).
          * - :py:attr:`~nftg`
            - Get or set the Field specifying the number of S-N curves to be defined.
          * - :py:attr:`~sntype`
            - Get or set the Stress type of S-N curve in fatigue analysis.
          * - :py:attr:`~texpos`
            - Get or set the Exposure time (used if option FATIGUE is used)
          * - :py:attr:`~strsf`
            - Get or set the Stress scale factor to accommodate different ordinates in S-N curve.
          * - :py:attr:`~inftg`
            - Get or set the Flag for including initial damage ratio.
          * - :py:attr:`~pid`
            - Get or set the Part ID, or Part Set ID, or Element (solid, shell, beam, thick shell) Set ID.
          * - :py:attr:`~lcid`
            - Get or set the S-N fatigue curve ID for the current Part or Part Set.
          * - :py:attr:`~ptype`
            - Get or set the Type of PID.
          * - :py:attr:`~ltype`
            - Get or set the Type of LCID.
          * - :py:attr:`~a`
            - Get or set the Material parameter a in S-N fatigue equation.
          * - :py:attr:`~b`
            - Get or set the Material parameter b in S-N fatigue equation.
          * - :py:attr:`~sthres`
            - Get or set the Fatigue threshold.
          * - :py:attr:`~snlimt`
            - Get or set the If LCID > 0 Flag setting algorithm used when stress is lower than the lowest stress on S-N curve (if LCID > 0), or lower than STHRES (if LCID < 0).
          * - :py:attr:`~filename`
            - Get or set the Path and name of existing binary database for fatigue information.


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

    from frequency_domain_random_vibration_fatigue import FrequencyDomainRandomVibrationFatigue

Property detail
---------------

.. py:property:: mdmin
   :type: int


   
   Get or set the The first mode in modal superposition method (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: mdmax
   :type: Optional[int]


   
   Get or set the The last mode in modal superposition method (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: fnmin
   :type: float


   
   Get or set the The minimum natural frequency in modal superposition method(optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: fnmax
   :type: Optional[float]


   
   Get or set the The maximum natural frequency in modal superposition method(optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: restrt
   :type: int


   
   Get or set the Restart option.
   EQ.0: A new modal analysis is performed,
   EQ.1: Restart with d3eigv.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: float


   
   Get or set the Modal damping coefficient, ζ.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdam
   :type: int


   
   Get or set the Load Curve ID defining mode dependent modal damping coefficient, ζ.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctyp
   :type: int


   
   Get or set the Type of load curve defining modal damping coefficient
   EQ.0: Abscissa value defines frequency,
   EQ.1: Abscissa value defines mode number.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmpmas
   :type: float


   
   Get or set the Mass proportional damping constant α, in Rayleigh damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmpstf
   :type: float


   
   Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmptyp
   :type: int


   
   Get or set the Type of damping
   EQ.0: modal damping.
   EQ.1: broadband damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: vaflag
   :type: int


   
   Get or set the Loading type:
   EQ.0: No random vibration analysis.
   EQ.1: Base acceleration.
   EQ.2: Random pressure.
   EQ.3: Plane wave.
   EQ.4: Shock wave.
   EQ.5: Progressive wave.
   EQ.6: Reverberant wave.
   EQ.7: Turbulent boundary layer wave.
   EQ.8: Nodal force.
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: int


   
   Get or set the method for modal response analysis.
   EQ.0: method set automatically by LS-DYNA (recommended)
   EQ.1: modal superposition method
   EQ.2: modal acceleration method
   EQ.3: modal truncation augmentation method.
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Flag for acceleration unit conversion:
   EQ.0: use [length unit]/[time unit]2 as unit of acceleration.
   EQ.1: use g as unit for acceleration, and SI units (Newton, kg, meter,
   second, etc.) elsewhere.
   EQ.2: use g as unit for acceleration, and Engineering units (lbf,
   lbf×second2/inch, inch, second, etc.) elsewhere.
   EQ.3:use g as unit for acceleration, and units (kN, kg, mm, ms, GPa, etc.) elsewhere.
   EQ.4:   Use g as unit for acceleration, and units (Newton, ton, mm, second, MPa, etc.) elsewhere.
   EQ.-1 use g as unit for acceleration and provide the multiplier for
   converting g to [length unit]/[time unit]2.
















   ..
       !! processed by numpydoc !!

.. py:property:: umlt
   :type: Optional[float]


   
   Get or set the Multiplier for converting g to [length unit]/[time unit]2(used only for UNIT = -1).
















   ..
       !! processed by numpydoc !!

.. py:property:: vapsd
   :type: int


   
   Get or set the Flag for PSD output:
   EQ.0: Absolute PSD output is requested.
   EQ.1: Relative PSD output is requested (used only for VAFLAG=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: varms
   :type: int


   
   Get or set the Flag for RMS output:
   EQ.0: Absolute RMS output is requested.
   EQ.1: Relative RMS output is requested (used only for VAFLAG=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: napsd
   :type: int


   
   Get or set the Number of auto PSD load definition. Card 5 is repeated "NAPSD" number of times,one for each auto PSD load definition. The default value is 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncpsd
   :type: int


   
   Get or set the Number of cross PSD load definition. Card 6 is repeated "NCPSD" times,one for each cross PSD load definition. The default value is 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldtyp
   :type: int


   
   Get or set the Excitation load (LDPSD in card 5) type:
   EQ.0: PSD.
   EQ.1: SPL (for plane wave only).
   EQ.2: time history load.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipanelu
   :type: Optional[int]


   
   Get or set the Number of strips in U direction (used only for VAFLAG=5, 6, 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: ipanelv
   :type: Optional[int]


   
   Get or set the Number of strips in V direction (used only for VAFLAG=5, 6, 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: temper
   :type: float


   
   Get or set the Temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldflag
   :type: int


   
   Get or set the Type of loading curves.
   EQ.0: Log-Log interpolation(default).
   EQ.1: Semi-Log interpolation.
   EQ.2: Linear-Linear interpolation.
















   ..
       !! processed by numpydoc !!

.. py:property:: icoarse
   :type: int


   
   Get or set the Option for PSD curve coarsening:
   EQ.0:   No coarsening, use original data (default).
   EQ.1:   Coarsening by keeping only peaks and troughs.
   EQ.2:   Coarsening by removing intermediate points whose slope change percentage is less than prescribed tolerance (TCOARSE).
















   ..
       !! processed by numpydoc !!

.. py:property:: tcoarse
   :type: float


   
   Get or set the Tolerance for slope change percentage for removing intermediate points from PSD curve (default is 0.1), for ICOARSE  =  2.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the GE.0: Set ID for the panel exposed to acoustic environment,or the nodes subjected to nodal force excitation,or nodal acceleration excitation.For VAFLAG = 1,base acceleration,leave this as blank
   LT.0: used to define the cross-PSD.|SID| is the ID of the load cases.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: Optional[int]


   
   Get or set the Flag specifying meaning of SID.
   EQ. 0: Node
   EQ. 1: Node Set
   EQ. 2: Segment Set
   EQ. 3: Part
   EQ. 4: Part Set
   LT.0: used to define the cross-psd.|STYPE| is the ID of the load cases.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom for nodal force excitation or base acceleration(DOF = 1, 2 and 3),or wave direction:
   EQ.0: translational movement in direction given by vector VID,
   EQ.1: x-translational degree-of-freedom,
   EQ.2: y-translational degree-of-freedom,
   EQ.3: z-translational degree-of-freedom.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldpsd
   :type: Optional[int]


   
   Get or set the Load curve for PSD, SPL or time history excitation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldvel
   :type: Optional[int]


   
   Get or set the Load curve for phase velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldflw
   :type: Optional[int]


   
   Get or set the Load curve for exponential decay for TBL in flow-wise direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldspn
   :type: Optional[int]


   
   Get or set the Load curve for exponential decay for TBL in span-wise direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for defining wave direction, see
   *DEFINE_COORDINATE_SYSTEM; or Vector ID for defining load
   direction for nodal force, or base excitation, see *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: load_i
   :type: Optional[int]


   
   Get or set the ID of load i for cross PSD.
















   ..
       !! processed by numpydoc !!

.. py:property:: load_j
   :type: Optional[int]


   
   Get or set the ID of load j for cross PSD.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctyp2
   :type: int


   
   Get or set the Type of load curves (LDPSD1 and LDPSD2) for defining cross PSD:
   EQ.0:LDPSD1 defines real part and LDPSD2 defines imaginary part
   EQ.1:LDPSD1 defines magnitude and LDPSD2 defines phase angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldpsd1
   :type: Optional[int]


   
   Get or set the Load curve for real part or magnitude of cross PSD.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldpsd2
   :type: Optional[int]


   
   Get or set the Load curve for imaginary part or phase angle of cross PSD.
















   ..
       !! processed by numpydoc !!

.. py:property:: mftg
   :type: int


   
   Get or set the Method for random fatigue analysis (for option _FATIGUE).
   EQ.0:no fatigue analysis,
   EQ.1:Steinberg's three-band method,
   EQ.2:Dirlik method,
   EQ.3:Narrow band method,
   EQ.4:Wirsching method,
   EQ.5:Chaudhury and Dover method,
   EQ.6:Tunna method,
   EQ.7:Hancock method.
















   ..
       !! processed by numpydoc !!

.. py:property:: nftg
   :type: int


   
   Get or set the Field specifying the number of S-N curves to be defined.
   GE.0: Number of S-N curves defined by card 8. Card 8 is repeated "NFTG" number of times, one for each S-N fatigue curve definition. The default value is 1.
   EQ.-999: S-N curves are defined through *MAT_ADD_FATIGUE.
   If the option FATIGUE is not used, ignore this parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: sntype
   :type: int


   
   Get or set the Stress type of S-N curve in fatigue analysis.
   EQ.0:von-mises stress
   EQ.1:maximum principal stress (not implemented)
   EQ.2:maximum shear stress (not implemented)
   EQ.-n:The nth stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: texpos
   :type: float


   
   Get or set the Exposure time (used if option FATIGUE is used)
















   ..
       !! processed by numpydoc !!

.. py:property:: strsf
   :type: float


   
   Get or set the Stress scale factor to accommodate different ordinates in S-N curve.
   EQ.1:used if the ordinate in S-N curve is stress range (default)
   EQ.2:used if the ordinate in S-N curve is stress amplitude
















   ..
       !! processed by numpydoc !!

.. py:property:: inftg
   :type: Optional[int]


   
   Get or set the Flag for including initial damage ratio.
   EQ.0: no initial damage ratio,
   GT.0: read existing d3ftg files to get initial damage ratio. When INFTG > 1, it means that the initial damage ratio comes from multiple loading cases (correspondingly, multiple binary databases, defined by Card 7). The value of INFTG should be<=10.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, or Part Set ID, or Element (solid, shell, beam, thick shell) Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the S-N fatigue curve ID for the current Part or Part Set.
   GT. 0: S-N fatigue curve ID
   EQ. -1: S-N fatigue curve uses equation N*S**b = a
   EQ. -2: S-N fatigue curve uses equation log(S) = a-b*log(N)
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the Type of PID.
   EQ. 0: Part (default)
   EQ. 1: Part Set
   EQ. 2: SET_SOLID
   EQ. 3: SET_BEAM
   EQ. 4: SET_SHELL
   EQ. 5: SET_TSHELL
















   ..
       !! processed by numpydoc !!

.. py:property:: ltype
   :type: int


   
   Get or set the Type of LCID.
   EQ. 0: Semi-log interpolation (default)
   EQ. 1: Log-Log interpolation
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Material parameter a in S-N fatigue equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Material parameter b in S-N fatigue equation.
















   ..
       !! processed by numpydoc !!

.. py:property:: sthres
   :type: float


   
   Get or set the Fatigue threshold.
















   ..
       !! processed by numpydoc !!

.. py:property:: snlimt
   :type: int


   
   Get or set the If LCID > 0 Flag setting algorithm used when stress is lower than the lowest stress on S-N curve (if LCID > 0), or lower than STHRES (if LCID < 0).
   EQ.0: use the life at the last point on S-N curve.
   EQ.1: extrapolation from the last two points on S-N curve (only applicable if LCID > 0).
   EQ.2: infinity.
   If LCID < 0 Flag setting algorithm used when stress is lower STHRES
   EQ.0: use the life at STHRES.
   EQ.1: Ingnored. only applicable for LCID > 0.
   EQ.2: infinity.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Path and name of existing binary database for fatigue information.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_RANDOM_VIBRATION_FATIGUE'






