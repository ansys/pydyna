





:class:`AirbagHybridChemkin`
============================


.. py:class:: airbag_hybrid_chemkin.AirbagHybridChemkin(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA AIRBAG_HYBRID_CHEMKIN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AirbagHybridChemkin

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID.
          * - :py:attr:`~sidtyp`
            - Get or set the Set type:
          * - :py:attr:`~rbid`
            - Get or set the Rigid body part ID for user defined activation subroutine:
          * - :py:attr:`~vsca`
            - Get or set the Volume scale factor, V-sca (default=1.0).
          * - :py:attr:`~psca`
            - Get or set the Pressure scale factor, P-sca (default=1.0).
          * - :py:attr:`~vini`
            - Get or set the Initial filled volume, V-ini (default=0.0).
          * - :py:attr:`~mwd`
            - Get or set the Mass weighted damping factor, D (default=0.0).
          * - :py:attr:`~spsf`
            - Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
          * - :py:attr:`~lcidm`
            - Get or set the Load curve specifying input mass flow rate versus time.
          * - :py:attr:`~lcidt`
            - Get or set the Load curve specifying input gas temperature versus time.
          * - :py:attr:`~ngas`
            - Get or set the Number of gas inputs to be defined below (including initial air).
          * - :py:attr:`~data`
            - Get or set the Thermodynamic database.
          * - :py:attr:`~atmt`
            - Get or set the Atmospheric temperature.
          * - :py:attr:`~atmp`
            - Get or set the Atmospheric pressure.
          * - :py:attr:`~rg`
            - Get or set the Universal gas constant.
          * - :py:attr:`~hconv`
            - Get or set the Convection heat transfer coefficient
          * - :py:attr:`~c23`
            - Get or set the Vent orifice coefficient
          * - :py:attr:`~a23`
            - Get or set the Vent orifice area
          * - :py:attr:`~chname`
            - Get or set the Chemical symbol for this gas species (e.g., N2 for nitrogen, AR for argon).
          * - :py:attr:`~mw`
            - Get or set the Molecular weight of this gas species.
          * - :py:attr:`~lcidn`
            - Get or set the Load curve specifying the input mole fraction versus time for this gas species. If >0, FMOLE is not used.
          * - :py:attr:`~fmole`
            - Get or set the Mole fraction of this gas species in the inlet stream.
          * - :py:attr:`~fmolet`
            - Get or set the Initial mole fraction of this gas species in the tank.
          * - :py:attr:`~tlow`
            - Get or set the Curve fit low temperature limit.
          * - :py:attr:`~tmid`
            - Get or set the Curve fit low-to-high transition temperature.
          * - :py:attr:`~thigh`
            - Get or set the Curve fit high temperature limit.
          * - :py:attr:`~alow`
            - Get or set the Low temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~blow`
            - Get or set the Low temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~clow`
            - Get or set the Low temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~dlow`
            - Get or set the Low temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~elow`
            - Get or set the Low temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~flow`
            - Get or set the Low temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~glow`
            - Get or set the Low temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~ahigh`
            - Get or set the High temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~bhigh`
            - Get or set the High temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~chigh`
            - Get or set the High temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~dhigh`
            - Get or set the High temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~ehigh`
            - Get or set the High temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~fhigh`
            - Get or set the High temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~ghigh`
            - Get or set the High temperature range NIST polynomial curve fit coefficient.
          * - :py:attr:`~a`
            - Get or set the Coefficient A, in the polynomial curve fit for heat capacity given by the equation:
          * - :py:attr:`~b`
            - Get or set the Coefficient B, in the polynomial curve fit for heat capacity given by the equation:
          * - :py:attr:`~c`
            - Get or set the Coefficient C, in the polynomial curve fit for heat capacity given by the equation:
          * - :py:attr:`~d`
            - Get or set the Coefficient D, in the polynomial curve fit for heat capacity given by the equation:
          * - :py:attr:`~e`
            - Get or set the Coefficient E, in the polynomial curve fit for heat capacity given by the equation:


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

    from airbag_hybrid_chemkin import AirbagHybridChemkin

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtyp
   :type: int


   
   Get or set the Set type:
   EQ.0: segment,
   EQ.1: part IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbid
   :type: int


   
   Get or set the Rigid body part ID for user defined activation subroutine:
   EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
   EQ.0: the control volume is active from time zero,
   EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
















   ..
       !! processed by numpydoc !!

.. py:property:: vsca
   :type: float


   
   Get or set the Volume scale factor, V-sca (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: psca
   :type: float


   
   Get or set the Pressure scale factor, P-sca (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: vini
   :type: float


   
   Get or set the Initial filled volume, V-ini (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: mwd
   :type: float


   
   Get or set the Mass weighted damping factor, D (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: spsf
   :type: float


   
   Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidm
   :type: Optional[int]


   
   Get or set the Load curve specifying input mass flow rate versus time.
   GT.0: piece wise linear interpolation
   LT.0: cubic spline interpolation
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Load curve specifying input gas temperature versus time.
   GT.0: piece wise linear interpolation
   LT.0: cubic spline interpolation
















   ..
       !! processed by numpydoc !!

.. py:property:: ngas
   :type: Optional[int]


   
   Get or set the Number of gas inputs to be defined below (including initial air).
















   ..
       !! processed by numpydoc !!

.. py:property:: data
   :type: int


   
   Get or set the Thermodynamic database.
   EQ.1: NIST database (3 additional property cards are required below),
   EQ.2: CHEMKIN database (no additional property cards are required),
   EQ.3: Polynomial data (1 additional property card is required below).
















   ..
       !! processed by numpydoc !!

.. py:property:: atmt
   :type: Optional[float]


   
   Get or set the Atmospheric temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: atmp
   :type: Optional[float]


   
   Get or set the Atmospheric pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: rg
   :type: Optional[float]


   
   Get or set the Universal gas constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: hconv
   :type: float


   
   Get or set the Convection heat transfer coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: float


   
   Get or set the Vent orifice coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: a23
   :type: float


   
   Get or set the Vent orifice area
















   ..
       !! processed by numpydoc !!

.. py:property:: chname
   :type: Optional[str]


   
   Get or set the Chemical symbol for this gas species (e.g., N2 for nitrogen, AR for argon).
   Required for DATA=2 (CHEMKIN), optional for DATA=1 or DATA=3.
















   ..
       !! processed by numpydoc !!

.. py:property:: mw
   :type: Optional[float]


   
   Get or set the Molecular weight of this gas species.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidn
   :type: int


   
   Get or set the Load curve specifying the input mole fraction versus time for this gas species. If >0, FMOLE is not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmole
   :type: Optional[float]


   
   Get or set the Mole fraction of this gas species in the inlet stream.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmolet
   :type: float


   
   Get or set the Initial mole fraction of this gas species in the tank.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlow
   :type: Optional[float]


   
   Get or set the Curve fit low temperature limit.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmid
   :type: Optional[float]


   
   Get or set the Curve fit low-to-high transition temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: thigh
   :type: Optional[float]


   
   Get or set the Curve fit high temperature limit.
















   ..
       !! processed by numpydoc !!

.. py:property:: alow
   :type: Optional[float]


   
   Get or set the Low temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: blow
   :type: Optional[float]


   
   Get or set the Low temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: clow
   :type: Optional[float]


   
   Get or set the Low temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlow
   :type: Optional[float]


   
   Get or set the Low temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: elow
   :type: Optional[float]


   
   Get or set the Low temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: flow
   :type: Optional[float]


   
   Get or set the Low temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: glow
   :type: Optional[float]


   
   Get or set the Low temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: ahigh
   :type: Optional[float]


   
   Get or set the High temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: bhigh
   :type: Optional[float]


   
   Get or set the High temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: chigh
   :type: Optional[float]


   
   Get or set the High temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: dhigh
   :type: Optional[float]


   
   Get or set the High temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: ehigh
   :type: Optional[float]


   
   Get or set the High temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: fhigh
   :type: Optional[float]


   
   Get or set the High temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: ghigh
   :type: Optional[float]


   
   Get or set the High temperature range NIST polynomial curve fit coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Coefficient A, in the polynomial curve fit for heat capacity given by the equation:
   c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: float


   
   Get or set the Coefficient B, in the polynomial curve fit for heat capacity given by the equation:
   c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: float


   
   Get or set the Coefficient C, in the polynomial curve fit for heat capacity given by the equation:
   c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: float


   
   Get or set the Coefficient D, in the polynomial curve fit for heat capacity given by the equation:
   c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: float


   
   Get or set the Coefficient E, in the polynomial curve fit for heat capacity given by the equation:
   c-p = 1/MW (A + BT + CT^2 + DT^3 + ET^4).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'AIRBAG'


.. py:attribute:: subkeyword
   :value: 'HYBRID_CHEMKIN'






