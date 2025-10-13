





:class:`FrequencyDomainResponseSpectrumDdam`
============================================


.. py:class:: frequency_domain_response_spectrum_ddam.FrequencyDomainResponseSpectrumDdam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_RESPONSE_SPECTRUM_DDAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainResponseSpectrumDdam

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
            - Get or set the The minimum natural frequency in modal superposition method (optional).
          * - :py:attr:`~fnmax`
            - Get or set the The maximum natural frequency in modal superposition method (optional).
          * - :py:attr:`~restrt`
            - Get or set the Restart option
          * - :py:attr:`~mcomb`
            - Get or set the Method for combination of modes:
          * - :py:attr:`~relatv`
            - Get or set the Type of nodal displacement, velocity and acceleration results:/n EQ.0: Relative values (with respect to the ground) are provided,/n EQ.1: Absolute values are provided.
          * - :py:attr:`~mprs`
            - Get or set the Multi-point or multidirectional response combination method:
          * - :py:attr:`~mcomb1`
            - Get or set the mode combination method for which results will be combined to the other combination method. It can have any value from the MCOMB description other than 99
          * - :py:attr:`~mcomb2`
            - Get or set the mode combination method for which results will be combined to the other combination method. It can have any value from the MCOMB description other than 99
          * - :py:attr:`~w1`
            - Get or set the Weight for the results given bythe MCOMB combination
          * - :py:attr:`~r40`
            - Get or set the Coefficient to replace 0.4 in 100-40-40 rule
          * - :py:attr:`~dampf`
            - Get or set the Modal damping ratio, ζ.
          * - :py:attr:`~lcdamp`
            - Get or set the Load Curve ID for defining frequency dependent modal damping ratio ζ.
          * - :py:attr:`~ldtyp`
            - Get or set the Type of load curve for LCDAMP
          * - :py:attr:`~dmpmas`
            - Get or set the Mass proportional damping constant α, in Rayleigh damping.
          * - :py:attr:`~dmpstf`
            - Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
          * - :py:attr:`~std`
            - Get or set the Design spectrum standard for shock load
          * - :py:attr:`~unit`
            - Get or set the Unit system
          * - :py:attr:`~amin`
            - Get or set the Minimum acceleration (in g - gravity acceleration).
          * - :py:attr:`~vid`
            - Get or set the Direction of shock load
          * - :py:attr:`~xc`
            - Get or set the X-directional cosine of shock load (if VID is undefined).
          * - :py:attr:`~yc`
            - Get or set the Y-directional cosine of shock load (if VID is undefined).
          * - :py:attr:`~zc`
            - Get or set the Z-directional cosine of shock load (if VID is undefined).
          * - :py:attr:`~effmas`
            - Get or set the Minimum percentage requirement of total modal mass.
          * - :py:attr:`~shptyp`
            - Get or set the Ship type
          * - :py:attr:`~mount`
            - Get or set the Mount type
          * - :py:attr:`~movemt`
            - Get or set the Movement type
          * - :py:attr:`~mattyp`
            - Get or set the Material type


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

    from frequency_domain_response_spectrum_ddam import FrequencyDomainResponseSpectrumDdam

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


   
   Get or set the The minimum natural frequency in modal superposition method (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: fnmax
   :type: Optional[float]


   
   Get or set the The maximum natural frequency in modal superposition method (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: restrt
   :type: int


   
   Get or set the Restart option
   EQ.0: A new run including modal analysis,
   EQ.1: Restart with d3eigv family files created elsewhere.
















   ..
       !! processed by numpydoc !!

.. py:property:: mcomb
   :type: int


   
   Get or set the Method for combination of modes:
   EQ.0: SRSS method,
   EQ.1: NRC Grouping method,
   EQ.2: Complete Quadratic Combination method (CQC),
   EQ.3: Double Sum method,
   EQ.4: NRL-SUM method,
   EQ.-4:  NRL-SUM method with CSM (Closely Spaced Modes) treatment.  The CSM pairs are automatically identified.
   EQ. - 14:       NRL - SUM method with CSM(Closely Spaced Modes) treatment, where the CSM pairs are defined by SID(Mode set ID, see * SET_MODE) in Card 5
   EQ.5: Double Sum method based on Gupta-Cordero coefficient,
   EQ.6: Double Sum method based on modified Gupta-Cordero coefficient,
   EQ.7: Rosenblueth method.
   EQ.8:   Absolute value method (ABS)
   EQ.99:combining results provided by two mode combination methods defined in Card 1.1 with corresponding weights defined in Card 1.2
















   ..
       !! processed by numpydoc !!

.. py:property:: relatv
   :type: int


   
   Get or set the Type of nodal displacement, velocity and acceleration results:/n EQ.0: Relative values (with respect to the ground) are provided,/n EQ.1: Absolute values are provided.
















   ..
       !! processed by numpydoc !!

.. py:property:: mprs
   :type: int


   
   Get or set the Multi-point or multidirectional response combination method:
   EQ.0:   SRSS.
   EQ.1 : 100 - 40 - 40 rule(Newmark method).
   EQ.2:   100-40-40 rule (Newmark method) with coefficient 0.4 replaced by R40 in Card 1a
















   ..
       !! processed by numpydoc !!

.. py:property:: mcomb1
   :type: int


   
   Get or set the mode combination method for which results will be combined to the other combination method. It can have any value from the MCOMB description other than 99
















   ..
       !! processed by numpydoc !!

.. py:property:: mcomb2
   :type: int


   
   Get or set the mode combination method for which results will be combined to the other combination method. It can have any value from the MCOMB description other than 99
















   ..
       !! processed by numpydoc !!

.. py:property:: w1
   :type: float


   
   Get or set the Weight for the results given bythe MCOMB combination
















   ..
       !! processed by numpydoc !!

.. py:property:: r40
   :type: float


   
   Get or set the Coefficient to replace 0.4 in 100-40-40 rule
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: Optional[float]


   
   Get or set the Modal damping ratio, ζ.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdamp
   :type: Optional[int]


   
   Get or set the Load Curve ID for defining frequency dependent modal damping ratio ζ.
















   ..
       !! processed by numpydoc !!

.. py:property:: ldtyp
   :type: int


   
   Get or set the Type of load curve for LCDAMP
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

.. py:property:: std
   :type: int


   
   Get or set the Design spectrum standard for shock load
   EQ.1:   NRL-1396,
   EQ.-1:Spectrum constants defined by user in Card 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Unit system
   EQ.1:   MKS (kg, m, s, N, Pa)
   EQ.2:   GPA (kg, mm, ms, kN, GPa)
   EQ.3:   MPA (ton, mm, s, N, MPa)
   EQ.4:   BIN (lb, in, s, lbf, psi)
   EQ.5    miu_MKS (gm, mm, ms, N, N/mm2)
   EQ.6:   CGS (gm, cm, s, dyne, dyne/cm2).
















   ..
       !! processed by numpydoc !!

.. py:property:: amin
   :type: float


   
   Get or set the Minimum acceleration (in g - gravity acceleration).
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the Direction of shock load
   EQ.1:   x-direction
   EQ.2:   y-direction
   EQ.3:   z-direction
   < 0:    direction is given by vector |VID|.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the X-directional cosine of shock load (if VID is undefined).
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the Y-directional cosine of shock load (if VID is undefined).
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the Z-directional cosine of shock load (if VID is undefined).
















   ..
       !! processed by numpydoc !!

.. py:property:: effmas
   :type: float


   
   Get or set the Minimum percentage requirement of total modal mass.
















   ..
       !! processed by numpydoc !!

.. py:property:: shptyp
   :type: int


   
   Get or set the Ship type
   EQ.1:   Submarine
   EQ.2:   Surface ship.
















   ..
       !! processed by numpydoc !!

.. py:property:: mount
   :type: int


   
   Get or set the Mount type
   EQ.1:   Hull Mounted System
   EQ.2:   Deck Mounted System
   EQ.3:   Shell Plating Mounted System.
















   ..
       !! processed by numpydoc !!

.. py:property:: movemt
   :type: int


   
   Get or set the Movement type
   EQ.1:   Vertical
   EQ.2:   Athwartship
   EQ.3:   Fore and Aft.
















   ..
       !! processed by numpydoc !!

.. py:property:: mattyp
   :type: int


   
   Get or set the Material type
   EQ.1:   Elastic
   EQ.2:   Elasto-plastic.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_RESPONSE_SPECTRUM_DDAM'






