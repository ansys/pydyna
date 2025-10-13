





:class:`MatPhsBmw`
==================


.. py:class:: mat_phs_bmw.MatPhsBmw(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_PHS_BMW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPhsBmw

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
            - Get or set the Material density at room temperature (necessary for calculating transformation induced strains).
          * - :py:attr:`~e`
            - Get or set the Youngs' modulus:
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio:
          * - :py:attr:`~tunit`
            - Get or set the Number of time units per hour. Default is seconds, that is 3600 time units per hour. It is used only for hardness calculations.
          * - :py:attr:`~trip`
            - Get or set the Flag to activate (0) or deactivate (1) trip effect calculation.
          * - :py:attr:`~phase`
            - Get or set the Switch to exclude middle phases from the simulation.
          * - :py:attr:`~heat`
            - Get or set the Heat flag as in MAT_244, see there for details.
          * - :py:attr:`~lcy1`
            - Get or set the Load curve or Table ID for austenite hardening.
          * - :py:attr:`~lcy2`
            - Get or set the Load curve or Table ID for ferrite. See LCY1 for description.
          * - :py:attr:`~lcy3`
            - Get or set the Load curve or Table ID for pearlite. See LCY1 for description.
          * - :py:attr:`~lcy4`
            - Get or set the Load curve or Table ID for bainite. See LCY1 for description.
          * - :py:attr:`~lcy5`
            - Get or set the Load curve or Table ID for martensite. See LCY1 for description.
          * - :py:attr:`~c_f`
            - Get or set the Alloy dependent factor  for ferrite (controls the alloying effects beside of Boron on the time-temperature-transformation start line of ferrite).
          * - :py:attr:`~c_p`
            - Get or set the Alloy dependent factor  for pearlite (see C_F for description).
          * - :py:attr:`~c_b`
            - Get or set the Alloy dependent factor  for bainite (see C_F for description).
          * - :py:attr:`~c`
            - Get or set the Carbon [weight %].
          * - :py:attr:`~co`
            - Get or set the Cobolt [weight %].
          * - :py:attr:`~mo`
            - Get or set the Molybdenum [weight %].
          * - :py:attr:`~cr`
            - Get or set the Chromium [weight %].
          * - :py:attr:`~ni`
            - Get or set the Nickel [weight %]
          * - :py:attr:`~mn`
            - Get or set the Manganese [weight %]
          * - :py:attr:`~si`
            - Get or set the Silicon [weight %]
          * - :py:attr:`~v`
            - Get or set the Vanadium [weight %]
          * - :py:attr:`~w`
            - Get or set the Tungsten [weight %].
          * - :py:attr:`~cu`
            - Get or set the Copper [weight %].
          * - :py:attr:`~p`
            - Get or set the Phosphorous [weight %].
          * - :py:attr:`~ai`
            - Get or set the Aluminium [weight %].
          * - :py:attr:`~as_`
            - Get or set the Arsenic [weight %]
          * - :py:attr:`~ti`
            - Get or set the Titanium [weight %]
          * - :py:attr:`~b`
            - Get or set the Boron [weight %]
          * - :py:attr:`~tabrho`
            - Get or set the Table definition for phase and temperature dependent densities. Needed for calculation of transformation induced strains.
          * - :py:attr:`~tref`
            - Get or set the Reference temperature for thermal expansion (only necessary for thermal expansion calculation with the secant method).
          * - :py:attr:`~lat1`
            - Get or set the Latent heat for the decomposition of austenite into ferrite, pearlite and bainite.
          * - :py:attr:`~lat5`
            - Get or set the Latent heat for the decomposition of austenite into martensite.
          * - :py:attr:`~tabth`
            - Get or set the Table definition for thermal expansion coefficient. See remarks for more information how to input this table.
          * - :py:attr:`~qr2`
            - Get or set the Activation energy divided by the universal gas constant for the diffusion reaction of the austenite-ferrite reaction: Q2/R. R= 8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate)
          * - :py:attr:`~qr3`
            - Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-pearlite reaction: Q3/R. R=8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate).
          * - :py:attr:`~qr4`
            - Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-bainite reaction: Q4/R. R=8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate).
          * - :py:attr:`~alpha`
            - Get or set the Material constant for the martensite phase. A value of 0.011 means that 90% of the available austenite is transformed into martensite at 210 degrees below the martensite start temperature (see message file for information), whereas a value of 0.033 means a 99.9% transformation.
          * - :py:attr:`~grain`
            - Get or set the ASTM grain size number  for austenite, usually a number between 7 and 11.
          * - :py:attr:`~toffe`
            - Get or set the Number of degrees that the ferrite is bleeding over into the pearlite reaction: .
          * - :py:attr:`~tofpe`
            - Get or set the Number of degrees that the pearlite is bleeding over into the bainite reaction: .
          * - :py:attr:`~tofba`
            - Get or set the Number of degrees that the bainite is bleeding over into the martensite reaction: .
          * - :py:attr:`~plmem2`
            - Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the ferrite phase and a value of 0 means that nothing is transferred.
          * - :py:attr:`~plmem3`
            - Get or set the Same as PLMEM2 but between austenite and pearlite.
          * - :py:attr:`~plmem4`
            - Get or set the Same as PLMEM2 but between austenite and bainite.
          * - :py:attr:`~plmem5`
            - Get or set the Same as PLMEM3 but between austenite and martensite.
          * - :py:attr:`~strc`
            - Get or set the Cowper and Symonds strain rate parameter .
          * - :py:attr:`~strp`
            - Get or set the Cowper and Symonds strain rate parameter P.
          * - :py:attr:`~fs`
            - Get or set the Manual start temperature ferrite, .
          * - :py:attr:`~ps`
            - Get or set the Manual start temperature pearlite, . See FS for description.
          * - :py:attr:`~bs`
            - Get or set the Manual start temperature bainite, . See FS for description.
          * - :py:attr:`~ms`
            - Get or set the Manual start temperature martensite, . See FS for description.
          * - :py:attr:`~msig`
            - Get or set the Describes the increase of martensite start temperature for cooling due to applied stress.
          * - :py:attr:`~lceps23`
            - Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR2 and QR3.
          * - :py:attr:`~lceps4`
            - Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR4.
          * - :py:attr:`~lceps5`
            - Get or set the Load Curve ID which describe the increase of the martensite start temperature for cooling as a function of plastic strain.       MS*= MS+ MSIG+LCEPS5()
          * - :py:attr:`~lch4`
            - Get or set the Load curve ID of Vickers hardness vs. temperature for bainite hardness calculation.
          * - :py:attr:`~lch5`
            - Get or set the Load curve ID of Vickers hardness vs. temperature for martensite hardness calculation.
          * - :py:attr:`~dtcrit`
            - Get or set the Critical cooling rate to detect holding phase.
          * - :py:attr:`~tsamp`
            - Get or set the Sampling interval for temperature rate monitoring to detect the holding phase
          * - :py:attr:`~islc`
            - Get or set the Flag for definition of evolution parameters on Cards 10 and 11.
          * - :py:attr:`~iextra`
            - Get or set the Flag to read extra cards (see Cards 14 and 15)
          * - :py:attr:`~alph_m`
            - Get or set the Martensite evolution parameter .
          * - :py:attr:`~n_m`
            - Get or set the Martensite evolution parameter.
          * - :py:attr:`~phi_m`
            - Get or set the Martensite evolution parameter .
          * - :py:attr:`~psi_m`
            - Get or set the Martensite evolution exponent , if  then .
          * - :py:attr:`~omg_f`
            - Get or set the Ferrite grain size factor  (mainly controls the alloying effect of Boron on the time-temperature-transformation start line of ferrite)
          * - :py:attr:`~phi_f`
            - Get or set the Ferrite evolution parameter  (controls the incubation time till 1vol% of ferrite is built)
          * - :py:attr:`~psi_f`
            - Get or set the Ferrite evolution parameter  (controls the time till 99vol% of ferrite is built without effect on the incubation time)
          * - :py:attr:`~cr_f`
            - Get or set the Ferrite evolution parameter  (retardation coefficient to influence the kinetics of phase transformation of ferrite, should be determined at slow cooling conditions, can also be defined in dependency to the cooling rate)
          * - :py:attr:`~omg_p`
            - Get or set the Pearlite grain size factor  (see OMG_F for description).
          * - :py:attr:`~phi_p`
            - Get or set the Pearlite evolution parameter  (see PHI_F for description).
          * - :py:attr:`~psi_p`
            - Get or set the Pearlite evolution parameter  (see PSI_F for description).
          * - :py:attr:`~cr_p`
            - Get or set the Pearlite evolution parameter  (see CR_F for description).
          * - :py:attr:`~omg_b`
            - Get or set the Bainite grain size factor  (see OMG_F for description)
          * - :py:attr:`~phi_b`
            - Get or set the Bainite evolution parameter  (see PHI_F for description)
          * - :py:attr:`~psi_b`
            - Get or set the Bainite evolution parameter  (see PSI_F for description)
          * - :py:attr:`~cr_b`
            - Get or set the Bainite evolution parameter  (see CR_F for description)
          * - :py:attr:`~aust`
            - Get or set the If a heating process is initiated at t = 0 this parameters sets the initial amount of austenite in the blank. If heating is activated at t > 0 during a simulation this value is ignored.
          * - :py:attr:`~ferr`
            - Get or set the See AUST for description.
          * - :py:attr:`~pear`
            - Get or set the See AUST for description.
          * - :py:attr:`~bain`
            - Get or set the See AUST for description.
          * - :py:attr:`~mart`
            - Get or set the See AUST for description
          * - :py:attr:`~grk`
            - Get or set the Growth parameter k (μm2/sec)
          * - :py:attr:`~grqr`
            - Get or set the Grain growth activation energy (J/mol) divided by the universal gas constant. Q/R where R=8.314472 (J/mol K)
          * - :py:attr:`~tau1`
            - Get or set the Empirical grain growth parameter  describing the function τ(T)
          * - :py:attr:`~gra`
            - Get or set the Grain growth parameter A.
          * - :py:attr:`~grb`
            - Get or set the Grain growth parameter B. A table of recommended values of GRA and GRB is included in Remark 7 of *MAT_244..
          * - :py:attr:`~expa`
            - Get or set the Grain growth parameter .
          * - :py:attr:`~expb`
            - Get or set the Grain growth parameter .
          * - :py:attr:`~grcc`
            - Get or set the Grain growth parameter with the concentration of non metals in the blank, weight% of C or N
          * - :py:attr:`~grcm`
            - Get or set the Grain growth parameter with the concentration of metals in the blank, lowest weight% of Cr, V, Nb, Ti, Al.
          * - :py:attr:`~heatn`
            - Get or set the Grain growth parameter  for the austenite formation
          * - :py:attr:`~tau2`
            - Get or set the Empirical grain growth parameter  describing the function τ(T)
          * - :py:attr:`~funca`
            - Get or set the ID of a *DEFINE_FUNCTION for saturation stress A (Hockett-Sherby approach)
          * - :py:attr:`~funcb`
            - Get or set the ID of a *DEFINE_FUNCTION for initial yield stress B (Hockett-Sherby approach)
          * - :py:attr:`~funcm`
            - Get or set the ID of a *DEFINE_FUNCTION for saturation rate M (Hockett-Sherby approach)
          * - :py:attr:`~tcvup`
            - Get or set the Upper temperature for determination of average cooling velocity
          * - :py:attr:`~tcvlo`
            - Get or set the Lower temperature for determination of average cooling velocity
          * - :py:attr:`~cvcrit`
            - Get or set the Critical cooling velocity. If the average cooling velocity is less than or equal to CVCRIT, the cooling rate at temperature TCVSL is used
          * - :py:attr:`~tcvsl`
            - Get or set the Temperature for determination of cooling velocity for small cooling velocities
          * - :py:attr:`~epsp`
            - Get or set the Plastic strain in Hockett-Sherby approach
          * - :py:attr:`~expon`
            - Get or set the Exponent in Hockett-Sherby approach
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

    from mat_phs_bmw import MatPhsBmw

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID, a unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Material density at room temperature (necessary for calculating transformation induced strains).
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Youngs' modulus:
   GT.0.0: constant value is used
   LT.0.0: LCID or TABID.  Temperature dependent Young's modulus given by load curve or table ID = -E. When using a table to describe the Young's modulus see Remark 10 for more information..
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio:
   GT.0.0: constant value is used
   LT.0.0: LCID or TABID.  Temperature dependent Poisson's ratio given by load curve or table ID = -PR. The table input is described in Remark 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: tunit
   :type: float


   
   Get or set the Number of time units per hour. Default is seconds, that is 3600 time units per hour. It is used only for hardness calculations.
















   ..
       !! processed by numpydoc !!

.. py:property:: trip
   :type: int


   
   Get or set the Flag to activate (0) or deactivate (1) trip effect calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: phase
   :type: int


   
   Get or set the Switch to exclude middle phases from the simulation.
   EQ.0:   all phases active (default)
   EQ.1:   pearlite and bainite active
   EQ.2:   bainite active
   EQ.3:   ferrite and pearlite active
   EQ.4:   ferrite and bainite active
   EQ.5:   no active middle phases (only austenite → martensite)
















   ..
       !! processed by numpydoc !!

.. py:property:: heat
   :type: int


   
   Get or set the Heat flag as in MAT_244, see there for details.
   EQ.0:   Heating is not activated.
   EQ.1:   Heating is activated.
   EQ.2:   Automatic switching between cooling and heating.
   LT.0:   Switch between cooling and heating is defined by a time dependent load curve with id ABS(HEAT).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy1
   :type: Optional[int]


   
   Get or set the Load curve or Table ID for austenite hardening.
   if LCID
   input yield stress versus effective plastic strain.
   if TABID.GT.0:
   2D table. Input temperatures as table values and hardening curves as targets for those temperatures (see *DEFINE_TABLE)
   if TABID.LT.0:
   3D table. Input temperatures as main table values and strain rates as values for the sub tables, and hardening curves as targets for those strain rates..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy2
   :type: Optional[int]


   
   Get or set the Load curve or Table ID for ferrite. See LCY1 for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy3
   :type: Optional[int]


   
   Get or set the Load curve or Table ID for pearlite. See LCY1 for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy4
   :type: Optional[int]


   
   Get or set the Load curve or Table ID for bainite. See LCY1 for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy5
   :type: Optional[int]


   
   Get or set the Load curve or Table ID for martensite. See LCY1 for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: c_f
   :type: Optional[float]


   
   Get or set the Alloy dependent factor  for ferrite (controls the alloying effects beside of Boron on the time-temperature-transformation start line of ferrite).
















   ..
       !! processed by numpydoc !!

.. py:property:: c_p
   :type: Optional[float]


   
   Get or set the Alloy dependent factor  for pearlite (see C_F for description).
















   ..
       !! processed by numpydoc !!

.. py:property:: c_b
   :type: Optional[float]


   
   Get or set the Alloy dependent factor  for bainite (see C_F for description).
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Carbon [weight %].
















   ..
       !! processed by numpydoc !!

.. py:property:: co
   :type: Optional[float]


   
   Get or set the Cobolt [weight %].
















   ..
       !! processed by numpydoc !!

.. py:property:: mo
   :type: Optional[float]


   
   Get or set the Molybdenum [weight %].
















   ..
       !! processed by numpydoc !!

.. py:property:: cr
   :type: Optional[float]


   
   Get or set the Chromium [weight %].
















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


   
   Get or set the Tungsten [weight %].
















   ..
       !! processed by numpydoc !!

.. py:property:: cu
   :type: Optional[float]


   
   Get or set the Copper [weight %].
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Phosphorous [weight %].
















   ..
       !! processed by numpydoc !!

.. py:property:: ai
   :type: Optional[float]


   
   Get or set the Aluminium [weight %].
















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

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Boron [weight %]
















   ..
       !! processed by numpydoc !!

.. py:property:: tabrho
   :type: Optional[int]


   
   Get or set the Table definition for phase and temperature dependent densities. Needed for calculation of transformation induced strains.
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: Optional[float]


   
   Get or set the Reference temperature for thermal expansion (only necessary for thermal expansion calculation with the secant method).
















   ..
       !! processed by numpydoc !!

.. py:property:: lat1
   :type: Optional[float]


   
   Get or set the Latent heat for the decomposition of austenite into ferrite, pearlite and bainite.
   GT.0.0: Constant value
   LT.0.0: Curve ID or Table ID: See remark 11 for more information.
















   ..
       !! processed by numpydoc !!

.. py:property:: lat5
   :type: Optional[float]


   
   Get or set the Latent heat for the decomposition of austenite into martensite.
   GT.0.0: Constant value
   LT.0.0: Curve ID:       Note that LAT 5 is ignored if a Table ID is used in LAT1.
















   ..
       !! processed by numpydoc !!

.. py:property:: tabth
   :type: Optional[int]


   
   Get or set the Table definition for thermal expansion coefficient. See remarks for more information how to input this table.
   GT.0:   A table for instantaneous thermal expansion (TREF is ignored).
   LT.0:   A table with thermal expansion with reference to TREF.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr2
   :type: Optional[float]


   
   Get or set the Activation energy divided by the universal gas constant for the diffusion reaction of the austenite-ferrite reaction: Q2/R. R= 8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate)
















   ..
       !! processed by numpydoc !!

.. py:property:: qr3
   :type: Optional[float]


   
   Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-pearlite reaction: Q3/R. R=8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate).
















   ..
       !! processed by numpydoc !!

.. py:property:: qr4
   :type: Optional[float]


   
   Get or set the Activation energy divided by the universal gas constant for the diffusion reaction for the austenite-bainite reaction: Q4/R. R=8.314472 [J/mol K].Load curve ID if ISLC=2 (function of cooling rate).
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Material constant for the martensite phase. A value of 0.011 means that 90% of the available austenite is transformed into martensite at 210 degrees below the martensite start temperature (see message file for information), whereas a value of 0.033 means a 99.9% transformation.
















   ..
       !! processed by numpydoc !!

.. py:property:: grain
   :type: Optional[float]


   
   Get or set the ASTM grain size number  for austenite, usually a number between 7 and 11.
















   ..
       !! processed by numpydoc !!

.. py:property:: toffe
   :type: Optional[float]


   
   Get or set the Number of degrees that the ferrite is bleeding over into the pearlite reaction: .
















   ..
       !! processed by numpydoc !!

.. py:property:: tofpe
   :type: Optional[float]


   
   Get or set the Number of degrees that the pearlite is bleeding over into the bainite reaction: .
















   ..
       !! processed by numpydoc !!

.. py:property:: tofba
   :type: Optional[float]


   
   Get or set the Number of degrees that the bainite is bleeding over into the martensite reaction: .
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem2
   :type: Optional[float]


   
   Get or set the Memory coefficient for the plastic strain that is carried over from the austenite. A value of 1 means that all plastic strains from austenite is transferred to the ferrite phase and a value of 0 means that nothing is transferred.
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem3
   :type: Optional[float]


   
   Get or set the Same as PLMEM2 but between austenite and pearlite.
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem4
   :type: Optional[float]


   
   Get or set the Same as PLMEM2 but between austenite and bainite.
















   ..
       !! processed by numpydoc !!

.. py:property:: plmem5
   :type: Optional[float]


   
   Get or set the Same as PLMEM3 but between austenite and martensite.
















   ..
       !! processed by numpydoc !!

.. py:property:: strc
   :type: Optional[float]


   
   Get or set the Cowper and Symonds strain rate parameter .
   STRC.LT.0.0:    load curve id = -STRC
   STRC.GT.0.0:    constant value
   STRC.EQ.0.0:    strain rate NOT active
















   ..
       !! processed by numpydoc !!

.. py:property:: strp
   :type: Optional[float]


   
   Get or set the Cowper and Symonds strain rate parameter P.
   STRP.LT.0.0:    load curve id = -STRP
   STRP.GT.0.0:    constant value
   STRP.EQ.0.0:    strain rate NOT active
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: Optional[float]


   
   Get or set the Manual start temperature ferrite, .
   GT.0.0: Same temperature is used for heating and cooling.
   LT.0.0: Curve ID:       Different start temperatures for cooling and heating given by load curve ID= -FS. First ordinate value is used for cooling, last ordinate value for heating..
















   ..
       !! processed by numpydoc !!

.. py:property:: ps
   :type: Optional[float]


   
   Get or set the Manual start temperature pearlite, . See FS for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: bs
   :type: Optional[float]


   
   Get or set the Manual start temperature bainite, . See FS for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: ms
   :type: Optional[float]


   
   Get or set the Manual start temperature martensite, . See FS for description.
















   ..
       !! processed by numpydoc !!

.. py:property:: msig
   :type: Optional[float]


   
   Get or set the Describes the increase of martensite start temperature for cooling due to applied stress.
   LT.0:   Load Curve ID describes MSIG as a function of triaxiality (pressure / effective stress).
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps23
   :type: Optional[int]


   
   Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR2 and QR3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps4
   :type: Optional[int]


   
   Get or set the Load Curve ID dependent on plastic strain that scales the activation energy QR4.
















   ..
       !! processed by numpydoc !!

.. py:property:: lceps5
   :type: Optional[int]


   
   Get or set the Load Curve ID which describe the increase of the martensite start temperature for cooling as a function of plastic strain.       MS*= MS+ MSIG+LCEPS5()
















   ..
       !! processed by numpydoc !!

.. py:property:: lch4
   :type: Optional[int]


   
   Get or set the Load curve ID of Vickers hardness vs. temperature for bainite hardness calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: lch5
   :type: Optional[int]


   
   Get or set the Load curve ID of Vickers hardness vs. temperature for martensite hardness calculation.
















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

.. py:property:: islc
   :type: int


   
   Get or set the Flag for definition of evolution parameters on Cards 10 and 11.
   EQ.0.0: All 16 fields on Cards 10 and 11 are constant values.
   EQ.1.0 : PHI_‌F, CR_‌F, PHI_‌P, CR_‌P, PHI_‌B,and CR_‌B are load curves defining values as functions of cooling rate.The remaining 10 fields on Cards 10 and 11 are constant values.
   EQ.2.0 : QR2, QR3, QR4 from Card 6 and allAll 16 fields on Cards 10 and 11 are load curves defining values as functions of cooling rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: iextra
   :type: int


   
   Get or set the Flag to read extra cards (see Cards 14 and 15)
















   ..
       !! processed by numpydoc !!

.. py:property:: alph_m
   :type: float


   
   Get or set the Martensite evolution parameter .
















   ..
       !! processed by numpydoc !!

.. py:property:: n_m
   :type: float


   
   Get or set the Martensite evolution parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: phi_m
   :type: float


   
   Get or set the Martensite evolution parameter .
















   ..
       !! processed by numpydoc !!

.. py:property:: psi_m
   :type: float


   
   Get or set the Martensite evolution exponent , if  then .
















   ..
       !! processed by numpydoc !!

.. py:property:: omg_f
   :type: float


   
   Get or set the Ferrite grain size factor  (mainly controls the alloying effect of Boron on the time-temperature-transformation start line of ferrite)
















   ..
       !! processed by numpydoc !!

.. py:property:: phi_f
   :type: float


   
   Get or set the Ferrite evolution parameter  (controls the incubation time till 1vol% of ferrite is built)
















   ..
       !! processed by numpydoc !!

.. py:property:: psi_f
   :type: float


   
   Get or set the Ferrite evolution parameter  (controls the time till 99vol% of ferrite is built without effect on the incubation time)
















   ..
       !! processed by numpydoc !!

.. py:property:: cr_f
   :type: Optional[float]


   
   Get or set the Ferrite evolution parameter  (retardation coefficient to influence the kinetics of phase transformation of ferrite, should be determined at slow cooling conditions, can also be defined in dependency to the cooling rate)
















   ..
       !! processed by numpydoc !!

.. py:property:: omg_p
   :type: float


   
   Get or set the Pearlite grain size factor  (see OMG_F for description).
















   ..
       !! processed by numpydoc !!

.. py:property:: phi_p
   :type: float


   
   Get or set the Pearlite evolution parameter  (see PHI_F for description).
















   ..
       !! processed by numpydoc !!

.. py:property:: psi_p
   :type: float


   
   Get or set the Pearlite evolution parameter  (see PSI_F for description).
















   ..
       !! processed by numpydoc !!

.. py:property:: cr_p
   :type: Optional[float]


   
   Get or set the Pearlite evolution parameter  (see CR_F for description).
















   ..
       !! processed by numpydoc !!

.. py:property:: omg_b
   :type: float


   
   Get or set the Bainite grain size factor  (see OMG_F for description)
















   ..
       !! processed by numpydoc !!

.. py:property:: phi_b
   :type: float


   
   Get or set the Bainite evolution parameter  (see PHI_F for description)
















   ..
       !! processed by numpydoc !!

.. py:property:: psi_b
   :type: float


   
   Get or set the Bainite evolution parameter  (see PSI_F for description)
















   ..
       !! processed by numpydoc !!

.. py:property:: cr_b
   :type: Optional[float]


   
   Get or set the Bainite evolution parameter  (see CR_F for description)
















   ..
       !! processed by numpydoc !!

.. py:property:: aust
   :type: Optional[float]


   
   Get or set the If a heating process is initiated at t = 0 this parameters sets the initial amount of austenite in the blank. If heating is activated at t > 0 during a simulation this value is ignored.
















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


   
   Get or set the See AUST for description
















   ..
       !! processed by numpydoc !!

.. py:property:: grk
   :type: Optional[float]


   
   Get or set the Growth parameter k (μm2/sec)
















   ..
       !! processed by numpydoc !!

.. py:property:: grqr
   :type: Optional[float]


   
   Get or set the Grain growth activation energy (J/mol) divided by the universal gas constant. Q/R where R=8.314472 (J/mol K)
















   ..
       !! processed by numpydoc !!

.. py:property:: tau1
   :type: float


   
   Get or set the Empirical grain growth parameter  describing the function τ(T)
















   ..
       !! processed by numpydoc !!

.. py:property:: gra
   :type: float


   
   Get or set the Grain growth parameter A.
















   ..
       !! processed by numpydoc !!

.. py:property:: grb
   :type: float


   
   Get or set the Grain growth parameter B. A table of recommended values of GRA and GRB is included in Remark 7 of *MAT_244..
















   ..
       !! processed by numpydoc !!

.. py:property:: expa
   :type: float


   
   Get or set the Grain growth parameter .
















   ..
       !! processed by numpydoc !!

.. py:property:: expb
   :type: float


   
   Get or set the Grain growth parameter .
















   ..
       !! processed by numpydoc !!

.. py:property:: grcc
   :type: Optional[float]


   
   Get or set the Grain growth parameter with the concentration of non metals in the blank, weight% of C or N
















   ..
       !! processed by numpydoc !!

.. py:property:: grcm
   :type: Optional[float]


   
   Get or set the Grain growth parameter with the concentration of metals in the blank, lowest weight% of Cr, V, Nb, Ti, Al.
















   ..
       !! processed by numpydoc !!

.. py:property:: heatn
   :type: float


   
   Get or set the Grain growth parameter  for the austenite formation
















   ..
       !! processed by numpydoc !!

.. py:property:: tau2
   :type: float


   
   Get or set the Empirical grain growth parameter  describing the function τ(T)
















   ..
       !! processed by numpydoc !!

.. py:property:: funca
   :type: Optional[int]


   
   Get or set the ID of a *DEFINE_FUNCTION for saturation stress A (Hockett-Sherby approach)
















   ..
       !! processed by numpydoc !!

.. py:property:: funcb
   :type: Optional[int]


   
   Get or set the ID of a *DEFINE_FUNCTION for initial yield stress B (Hockett-Sherby approach)
















   ..
       !! processed by numpydoc !!

.. py:property:: funcm
   :type: Optional[int]


   
   Get or set the ID of a *DEFINE_FUNCTION for saturation rate M (Hockett-Sherby approach)
















   ..
       !! processed by numpydoc !!

.. py:property:: tcvup
   :type: float


   
   Get or set the Upper temperature for determination of average cooling velocity
















   ..
       !! processed by numpydoc !!

.. py:property:: tcvlo
   :type: float


   
   Get or set the Lower temperature for determination of average cooling velocity
















   ..
       !! processed by numpydoc !!

.. py:property:: cvcrit
   :type: float


   
   Get or set the Critical cooling velocity. If the average cooling velocity is less than or equal to CVCRIT, the cooling rate at temperature TCVSL is used
















   ..
       !! processed by numpydoc !!

.. py:property:: tcvsl
   :type: float


   
   Get or set the Temperature for determination of cooling velocity for small cooling velocities
















   ..
       !! processed by numpydoc !!

.. py:property:: epsp
   :type: float


   
   Get or set the Plastic strain in Hockett-Sherby approach
















   ..
       !! processed by numpydoc !!

.. py:property:: expon
   :type: float


   
   Get or set the Exponent in Hockett-Sherby approach
















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
   :value: 'PHS_BMW'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





