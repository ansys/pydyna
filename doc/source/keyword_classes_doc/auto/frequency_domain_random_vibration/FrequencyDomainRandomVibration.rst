





:class:`FrequencyDomainRandomVibration`
=======================================


.. py:class:: frequency_domain_random_vibration.FrequencyDomainRandomVibration(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_RANDOM_VIBRATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainRandomVibration

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

    from frequency_domain_random_vibration import FrequencyDomainRandomVibration

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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_RANDOM_VIBRATION'






