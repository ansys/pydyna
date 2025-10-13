





:class:`MatUhsSteel`
====================


.. py:class:: mat_uhs_steel.MatUhsSteel(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_UHS_STEEL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatUhsSteel

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID, a unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Material density
          * - :py:attr:`~e`
            - Get or set the Young's modulus:
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio
          * - :py:attr:`~tunit`
            - Get or set the Number of time units per hour. Default is seconds, that is 3600 time units per hour. It is used only for hardness calculations.
          * - :py:attr:`~crsh`
            - Get or set the Switch to use a simple and fast material model but with the actual phases active.
          * - :py:attr:`~phase`
            - Get or set the Switch to exclude middle phases from the simulation.
          * - :py:attr:`~heat`
            - Get or set the Switch to activate the heating algorithms
          * - :py:attr:`~lcy1`
            - Get or set the Load curve or Table ID for austenite hardening.
          * - :py:attr:`~lcy2`
            - Get or set the Load curve ID for ferrite hardening (stress versus eff. pl. str.)
          * - :py:attr:`~lcy3`
            - Get or set the Load curve ID for pearlite hardening (stress versus eff. pl. str.)
          * - :py:attr:`~lcy4`
            - Get or set the Load curve ID for bainite hardening (stress versus eff. pl. str.)
          * - :py:attr:`~lcy5`
            - Get or set the Load curve ID for martensite hardening (stress versus eff. pl. str.)
          * - :py:attr:`~kfer`
            - Get or set the Correction factor for boron in the ferrite reaction.
          * - :py:attr:`~kper`
            - Get or set the Correction factor for boron in the pearlite reaction.
          * - :py:attr:`~b`
            - Get or set the Boron [weight %]
          * - :py:attr:`~c`
            - Get or set the Carbon [weight %]
          * - :py:attr:`~co`
            - Get or set the Cobolt [weight %]
          * - :py:attr:`~mo`
            - Get or set the Molybdenum [weight %]
          * - :py:attr:`~cr`
            - Get or set the Chromium [weight %]
          * - :py:attr:`~ni`
            - Get or set the Nickel [weight %]
          * - :py:attr:`~mn`
            - Get or set the Manganese [weight %]
          * - :py:attr:`~si`
            - Get or set the Silicon [weight %]
          * - :py:attr:`~v`
            - Get or set the Vanadium [weight %]
          * - :py:attr:`~w`
            - Get or set the Tungsten [weight %]
          * - :py:attr:`~cu`
            - Get or set the copper [weight %]
          * - :py:attr:`~p`
            - Get or set the Phosphorous [weight %]
          * - :py:attr:`~al`
            - Get or set the Aluminium [weight %]
          * - :py:attr:`~as_`
            - Get or set the Arsenic [weight %]
          * - :py:attr:`~ti`
            - Get or set the Titanium [weight %]
          * - :py:attr:`~cwm`
            - Get or set the Flag for computational welding mechanics input. One additional input card is read.
          * - :py:attr:`~lctre`
            - Get or set the Load curve for transformation induced strains. See Remark 13 for more information.
          * - :py:attr:`~thexp1`
            - Get or set the Coefficient of thermal expansion in austenite
          * - :py:attr:`~thexp5`
            - Get or set the Coefficient of thermal expansion in martensite
          * - :py:attr:`~lcth1`
            - Get or set the Load curve for the thermal expansion coefficient for austenite:
          * - :py:attr:`~lcth5`
            - Get or set the Load curve for the thermal expansion coefficient for martensite:
          * - :py:attr:`~tref`
            - Get or set the Reference temperature for thermal expansion. Used if and only if LA.LT.0.0 or/and LM.LT.0.0
          * - :py:attr:`~lat1`
            - Get or set the Latent heat for the decomposition of austenite into ferrite, pearlite and bainite.
          * - :py:attr:`~lat5`
            - Get or set the Latent heat for the decomposition of austenite into martensite
          * - :py:attr:`~tabth`
            - Get or set the Table definition for thermal expansion coefficient. With this option active THEXP1,
          * - :py:attr:`~qr2`
            - Get or set the Activation energy divided by the universal gas constant for the diffusion reaction of the austenite-ferrite reaction: Q2/R. R = 8.314472 [J/mol K].
          * - :py:attr:`~qr3`
            - Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-pearlite reaction: Q3/R. R=8.314472 [J/mol K].
          * - :py:attr:`~qr4`
            - Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-bainite reaction: Q4/R. R=8.314472 [J/mol K].
          * - :py:attr:`~alpha`
            - Get or set the Material constant for the martensite phase. A value of 0.011 means that 90% of the available austenite is transformed into martensite at 210 degrees below TSMART, whereas a value of 0.033 means a 99.9% transformation.
          * - :py:attr:`~grain`
            - Get or set the ASTM grain size number for austenite, usually a number between 7 and 11.
          * - :py:attr:`~toffe`
            - Get or set the Number of degrees that the ferrite is bleeding over into the pearlite reaction.
          * - :py:attr:`~tofpe`
            - Get or set the Number of degrees that the pearlite is bleeding over into the bainite reaction.
          * - :py:attr:`~tofba`
            - Get or set the Number of degrees that the bainite is bleeding over into the martensite reaction.
          * - :py:attr:`~plmem2`
            - Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the ferrite phase and a value of 0 means that nothing is transferred.
          * - :py:attr:`~plmem3`
            - Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the pearlite phase and a value of 0 means that nothing is transferred.
          * - :py:attr:`~plmem4`
            - Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the bainite phase and a value of 0 means that nothing is transferred.
          * - :py:attr:`~plmem5`
            - Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the martensite phase and a value of 0 means that nothing is transferred.
          * - :py:attr:`~strc`
            - Get or set the Effective strain rate parameter C.
          * - :py:attr:`~strp`
            - Get or set the Effective strain rate parameter P.
          * - :py:attr:`~react`
            - Get or set the Flag for advanced reaction kinetics input.
          * - :py:attr:`~temper`
            - Get or set the Flag for tempering input. One additional input card is read.
          * - :py:attr:`~aust`
            - Get or set the If a heating process is initiated at t = 0 this
          * - :py:attr:`~ferr`
            - Get or set the See AUST for description.
          * - :py:attr:`~pear`
            - Get or set the See AUST for description.
          * - :py:attr:`~bain`
            - Get or set the See AUST for description.
          * - :py:attr:`~mart`
            - Get or set the See AUST for description.
          * - :py:attr:`~grk`
            - Get or set the Growth parameter k (μm2/sec).
          * - :py:attr:`~grqr`
            - Get or set the Grain growth activation energy (J/mol)
          * - :py:attr:`~tau1`
            - Get or set the Empirical grain growth parameter 𝑐1 describing the function τ(T).
          * - :py:attr:`~gra`
            - Get or set the Grain growth parameter A.
          * - :py:attr:`~grb`
            - Get or set the Grain growth parameter B. A table of
          * - :py:attr:`~expa`
            - Get or set the Grain growth parameter a.
          * - :py:attr:`~expb`
            - Get or set the Grain growth parameter b.
          * - :py:attr:`~grcc`
            - Get or set the Grain growth parameter with the concentration of non metals in the blank, weight% of C or N.
          * - :py:attr:`~grcm`
            - Get or set the Grain growth parameter with the concentration of metals in the blank, lowest weight% of Cr, V, Nb, Ti, Al.
          * - :py:attr:`~heatn`
            - Get or set the Grain growth parameter n for the austenite formation.
          * - :py:attr:`~tau2`
            - Get or set the Empirical grain growth parameter 𝑐2 describing the function τ(T).
          * - :py:attr:`~fs`
            - Get or set the Manual start temperature Ferrite
          * - :py:attr:`~ps`
            - Get or set the Manual start temperature Pearlite. See FS for description.
          * - :py:attr:`~bs`
            - Get or set the Manual start temperature Bainite. See BS for description.
          * - :py:attr:`~ms`
            - Get or set the Manual start temperature Martensite. See MS for description.
          * - :py:attr:`~msig`
            - Get or set the Describes the increase of martensite start temperature for cooling due to applied stress.
          * - :py:attr:`~lceps23`
            - Get or set the Load Curve ID dependent on plastic strain
          * - :py:attr:`~lceps4`
            - Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR4.n        QR4 = Q4 x LCEPS4(𝜀pl) / R.
          * - :py:attr:`~lceps5`
            - Get or set the Load Curve ID which describe the increase
          * - :py:attr:`~lch4`
            - Get or set the Load curve ID of Vicker hardness vs. temperature for Bainite hardness calculation.
          * - :py:attr:`~lch5`
            - Get or set the Load curve ID of Vicker hardness vs. temperature for Martensite hardness calculation.
          * - :py:attr:`~dtcrit`
            - Get or set the Critical cooling rate to detect holding phase.
          * - :py:attr:`~tsamp`
            - Get or set the Sampling interval for temperature rate monitoring to detect the holding phase
          * - :py:attr:`~tastart`
            - Get or set the Annealing temperature start
          * - :py:attr:`~taend`
            - Get or set the Annealing temperature end
          * - :py:attr:`~tlstart`
            - Get or set the Birth temperature start
          * - :py:attr:`~tlend`
            - Get or set the Birth temperature end.
          * - :py:attr:`~eghost`
            - Get or set the Young's modulus for ghost (quiet) material.
          * - :py:attr:`~pghost`
            - Get or set the Poisson's ratio for ghost (quiet) material
          * - :py:attr:`~aghost`
            - Get or set the Thermal expansion coefficient for ghost (quiet) material.
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

    from mat_uhs_steel import MatUhsSteel

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID, a unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Material density
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus:
   GT.0.0: constant value is used
   LT.0.0: temperature dependent Young's modulus given by load curve ID = -E
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: tunit
   :type: Optional[float]


   
   Get or set the Number of time units per hour. Default is seconds, that is 3600 time units per hour. It is used only for hardness calculations.
















   ..
       !! processed by numpydoc !!

.. py:property:: crsh
   :type: int


   
   Get or set the Switch to use a simple and fast material model but with the actual phases active.
   EQ.0: The original model were phase transitions and trip is used.
   EQ.1: A more simpler and faster version is active. To use this the NIPS and/or NIPH on *DATABASE_EXTENT_BINARY must be set to 12 or greater. Please see remark 5 below for more information.
















   ..
       !! processed by numpydoc !!

.. py:property:: phase
   :type: int


   
   Get or set the Switch to exclude middle phases from the simulation.
   EQ.0: All phases ACTIVE default)
   EQ.1: pearlite and bainite ACTIVE
   EQ.2: bainite ACTIVE
   EQ.3: ferrite and pearlite ACTIVE
   EQ.4: ferrite and bainite ACTIVE
   EQ.5: NO ACTIVE middle phases (only austenite -> martensite)
















   ..
       !! processed by numpydoc !!

.. py:property:: heat
   :type: int


   
   Get or set the Switch to activate the heating algorithms
   EQ.0: Heating is not activated. That means that no transformation to Austenite is possible.
   EQ.1: Heating is activated: That means that only transformation to Austenite is possible.
   EQ.2: Automatic switching between cooling and heating. LS-DYNA checks the temperature gradient and calls the appropriate algorithms.
   For example, this can be used to simulate the heat affected zone during welding.
   LT.0: Switch between cooling and heating is defined by a time dependent load curve with id
   ABS(HEAT). The ordinate should be 1.0 when heating is applied and 0.0 if cooling is preferable.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy1
   :type: Optional[int]


   
   Get or set the Load curve or Table ID for austenite hardening.
   IF LCID input yield stress versus effective plastic strain. IF TABID.
   GT.0: 2D table. Input temperatures as table values and hardening curves as targets
   for those temperatures (see *DEFINE_TABLE) IF TABID.
   LT.0: 3D table. Input temperatures as main table values and strain rates as values
   for the sub tables, and hardening curves as targets for those strain rates.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy2
   :type: Optional[int]


   
   Get or set the Load curve ID for ferrite hardening (stress versus eff. pl. str.)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy3
   :type: Optional[int]


   
   Get or set the Load curve ID for pearlite hardening (stress versus eff. pl. str.)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy4
   :type: Optional[int]


   
   Get or set the Load curve ID for bainite hardening (stress versus eff. pl. str.)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy5
   :type: Optional[int]


   
   Get or set the Load curve ID for martensite hardening (stress versus eff. pl. str.)
















   ..
       !! processed by numpydoc !!

.. py:property:: kfer
   :type: Optional[float]


   
   Get or set the Correction factor for boron in the ferrite reaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: kper
   :type: Optional[float]


   
   Get or set the Correction factor for boron in the pearlite reaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Boron [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Carbon [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: co
   :type: Optional[float]


   
   Get or set the Cobolt [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: mo
   :type: Optional[float]


   
   Get or set the Molybdenum [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: cr
   :type: Optional[float]


   
   Get or set the Chromium [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: ni
   :type: Optional[float]


   
   Get or set the Nickel [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: mn
   :type: Optional[float]


   
   Get or set the Manganese [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: si
   :type: Optional[float]


   
   Get or set the Silicon [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: v
   :type: Optional[float]


   
   Get or set the Vanadium [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Tungsten [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: cu
   :type: Optional[float]


   
   Get or set the copper [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Phosphorous [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: al
   :type: Optional[float]


   
   Get or set the Aluminium [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: as_
   :type: Optional[float]


   
   Get or set the Arsenic [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: ti
   :type: Optional[float]


   
   Get or set the Titanium [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: cwm
   :type: int


   
   Get or set the Flag for computational welding mechanics input. One additional input card is read.
   EQ.1.0: Active
   EQ.0.0: Inactive
















   ..
       !! processed by numpydoc !!

.. py:property:: lctre
   :type: Optional[int]


   
   Get or set the Load curve for transformation induced strains. See Remark 13 for more information.
















   ..
       !! processed by numpydoc !!

.. py:property:: thexp1
   :type: Optional[float]


   
   Get or set the Coefficient of thermal expansion in austenite
















   ..
       !! processed by numpydoc !!

.. py:property:: thexp5
   :type: Optional[float]


   
   Get or set the Coefficient of thermal expansion in martensite
















   ..
       !! processed by numpydoc !!

.. py:property:: lcth1
   :type: Optional[int]


   
   Get or set the Load curve for the thermal expansion coefficient for austenite:
   LT.0.0: curve ID = -LA and TREF is used as reference temperature
   GT.0.0: curve ID = LA
















   ..
       !! processed by numpydoc !!

.. py:property:: lcth5
   :type: Optional[int]


   
   Get or set the Load curve for the thermal expansion coefficient for martensite:
   LT.0.0: curve ID = -LA and TREF is used as reference temperature
   GT.0.0: curve ID = LA
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: float


   
   Get or set the Reference temperature for thermal expansion. Used if and only if LA.LT.0.0 or/and LM.LT.0.0
















   ..
       !! processed by numpydoc !!

.. py:property:: lat1
   :type: Optional[float]


   
   Get or set the Latent heat for the decomposition of austenite into ferrite, pearlite and bainite.
















   ..
       !! processed by numpydoc !!

.. py:property:: lat5
   :type: Optional[float]


   
   Get or set the Latent heat for the decomposition of austenite into martensite
















   ..
       !! processed by numpydoc !!

.. py:property:: tabth
   :type: Optional[int]


   
   Get or set the Table definition for thermal expansion coefficient. With this option active THEXP1,
   THEXP2, LCTH1 and LCTH5 are ignored. See remarks for more information how to input this table.
   GT.0: A table for instantaneous thermal expansion (TREF is ignored).
   LT.0: A table with thermal expansion with reference to TREF.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr2
   :type: Optional[float]


   
   Get or set the Activation energy divided by the universal gas constant for the diffusion reaction of the austenite-ferrite reaction: Q2/R. R = 8.314472 [J/mol K].
















   ..
       !! processed by numpydoc !!

.. py:property:: qr3
   :type: Optional[float]


   
   Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-pearlite reaction: Q3/R. R=8.314472 [J/mol K].
















   ..
       !! processed by numpydoc !!

.. py:property:: qr4
   :type: Optional[float]


   
   Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-bainite reaction: Q4/R. R=8.314472 [J/mol K].
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Material constant for the martensite phase. A value of 0.011 means that 90% of the available austenite is transformed into martensite at 210 degrees below TSMART, whereas a value of 0.033 means a 99.9% transformation.
















   ..
       !! processed by numpydoc !!

.. py:property:: grain
   :type: Optional[float]


   
   Get or set the ASTM grain size number for austenite, usually a number between 7 and 11.
















   ..
       !! processed by numpydoc !!

.. py:property:: toffe
   :type: Optional[float]


   
   Get or set the Number of degrees that the ferrite is bleeding over into the pearlite reaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: tofpe
   :type: Optional[float]


   
   Get or set the Number of degrees that the pearlite is bleeding over into the bainite reaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: tofba
   :type: Optional[float]


   
   Get or set the Number of degrees that the bainite is bleeding over into the martensite reaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem2
   :type: Optional[float]


   
   Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the ferrite phase and a value of 0 means that nothing is transferred.
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem3
   :type: Optional[float]


   
   Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the pearlite phase and a value of 0 means that nothing is transferred.
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem4
   :type: Optional[float]


   
   Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the bainite phase and a value of 0 means that nothing is transferred.
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem5
   :type: Optional[float]


   
   Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the martensite phase and a value of 0 means that nothing is transferred.
















   ..
       !! processed by numpydoc !!

.. py:property:: strc
   :type: Optional[float]


   
   Get or set the Effective strain rate parameter C.
   STRC.LT.0.0: load curve id = -STRC
   STRC.GT.0.0: constant value
   STRC.EQ.0.0: strain rate NOT active
















   ..
       !! processed by numpydoc !!

.. py:property:: strp
   :type: Optional[float]


   
   Get or set the Effective strain rate parameter P.
   STRP.LT.0.0: load curve id = -STRP
   STRP.GT.0.0: constant value
   STRP.EQ.0.0: strain rate NOT active
















   ..
       !! processed by numpydoc !!

.. py:property:: react
   :type: int


   
   Get or set the Flag for advanced reaction kinetics input.
   One additional input card is read.
   EQ.1.0: Active
   EQ.0.0: Inactive
















   ..
       !! processed by numpydoc !!

.. py:property:: temper
   :type: int


   
   Get or set the Flag for tempering input. One additional input card is read.
   EQ.1.0: Active
   EQ.0.0: Inactive
















   ..
       !! processed by numpydoc !!

.. py:property:: aust
   :type: Optional[float]


   
   Get or set the If a heating process is initiated at t = 0 this
   parameters sets the initial amount of austenite in the blank. If heating is
   activated at t > 0 during a simulation this value is ignored. Note that,
   AUST + FERR + PEAR + BAIN + MART        = 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: ferr
   :type: Optional[float]


   
   Get or set the See AUST for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: pear
   :type: Optional[float]


   
   Get or set the See AUST for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: bain
   :type: Optional[float]


   
   Get or set the See AUST for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: mart
   :type: Optional[float]


   
   Get or set the See AUST for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: grk
   :type: Optional[float]


   
   Get or set the Growth parameter k (μm2/sec).
















   ..
       !! processed by numpydoc !!

.. py:property:: grqr
   :type: Optional[float]


   
   Get or set the Grain growth activation energy (J/mol)
   divided by the universal gas constant. Q/R where R = 8.314472 (J/mol K).
















   ..
       !! processed by numpydoc !!

.. py:property:: tau1
   :type: float


   
   Get or set the Empirical grain growth parameter 𝑐1 describing the function τ(T).
















   ..
       !! processed by numpydoc !!

.. py:property:: gra
   :type: float


   
   Get or set the Grain growth parameter A.
















   ..
       !! processed by numpydoc !!

.. py:property:: grb
   :type: float


   
   Get or set the Grain growth parameter B. A table of
   recommended values of GRA and GRB is included in Remark 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: expa
   :type: float


   
   Get or set the Grain growth parameter a.
















   ..
       !! processed by numpydoc !!

.. py:property:: expb
   :type: float


   
   Get or set the Grain growth parameter b.
















   ..
       !! processed by numpydoc !!

.. py:property:: grcc
   :type: Optional[float]


   
   Get or set the Grain growth parameter with the concentration of non metals in the blank, weight% of C or N.
















   ..
       !! processed by numpydoc !!

.. py:property:: grcm
   :type: Optional[float]


   
   Get or set the Grain growth parameter with the concentration of metals in the blank, lowest weight% of Cr, V, Nb, Ti, Al.
















   ..
       !! processed by numpydoc !!

.. py:property:: heatn
   :type: float


   
   Get or set the Grain growth parameter n for the austenite formation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tau2
   :type: float


   
   Get or set the Empirical grain growth parameter 𝑐2 describing the function τ(T).
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: Optional[float]


   
   Get or set the Manual start temperature Ferrite
   GT.0.0: Same temperature is used for heating and cooling.
   LT.0.0: Curve ID: Different start temperatures for cooling and heating given by load curve
   ID = -FS. First ordinate value is used for cooling, last ordinate value for heating.
















   ..
       !! processed by numpydoc !!

.. py:property:: ps
   :type: Optional[float]


   
   Get or set the Manual start temperature Pearlite. See FS for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: bs
   :type: Optional[float]


   
   Get or set the Manual start temperature Bainite. See BS for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: ms
   :type: Optional[float]


   
   Get or set the Manual start temperature Martensite. See MS for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: msig
   :type: Optional[float]


   
   Get or set the Describes the increase of martensite start temperature for cooling due to applied stress.
   LT.0: Load Curve ID describes MSIG as a function of triaxiality (pressure /     effective stress).
   MS* = MS + MSIG x SIGMAeff.
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps23
   :type: Optional[int]


   
   Get or set the Load Curve ID dependent on plastic strain
   that scales the activation energy QR2 and QR3.
   QRx = Qx x CEPS23(𝜀pl) / R
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps4
   :type: Optional[int]


   
   Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR4.n        QR4 = Q4 x LCEPS4(𝜀pl) / R.
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps5
   :type: Optional[int]


   
   Get or set the Load Curve ID which describe the increase
   of the martensite start temperature for cooling as a function of plastic strain.
   MS* = MS + MSIG x 𝜎eff + LCEPS5(𝜀pl).
















   ..
       !! processed by numpydoc !!

.. py:property:: lch4
   :type: int


   
   Get or set the Load curve ID of Vicker hardness vs. temperature for Bainite hardness calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: lch5
   :type: int


   
   Get or set the Load curve ID of Vicker hardness vs. temperature for Martensite hardness calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtcrit
   :type: Optional[float]


   
   Get or set the Critical cooling rate to detect holding phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsamp
   :type: Optional[float]


   
   Get or set the Sampling interval for temperature rate monitoring to detect the holding phase
















   ..
       !! processed by numpydoc !!

.. py:property:: tastart
   :type: Optional[float]


   
   Get or set the Annealing temperature start
















   ..
       !! processed by numpydoc !!

.. py:property:: taend
   :type: Optional[float]


   
   Get or set the Annealing temperature end
















   ..
       !! processed by numpydoc !!

.. py:property:: tlstart
   :type: Optional[float]


   
   Get or set the Birth temperature start
















   ..
       !! processed by numpydoc !!

.. py:property:: tlend
   :type: Optional[float]


   
   Get or set the Birth temperature end.
















   ..
       !! processed by numpydoc !!

.. py:property:: eghost
   :type: Optional[float]


   
   Get or set the Young's modulus for ghost (quiet) material.
















   ..
       !! processed by numpydoc !!

.. py:property:: pghost
   :type: Optional[float]


   
   Get or set the Poisson's ratio for ghost (quiet) material
















   ..
       !! processed by numpydoc !!

.. py:property:: aghost
   :type: Optional[float]


   
   Get or set the Thermal expansion coefficient for ghost (quiet) material.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'UHS_STEEL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





