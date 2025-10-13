





:class:`FrequencyDomainResponseSpectrum`
========================================


.. py:class:: frequency_domain_response_spectrum.FrequencyDomainResponseSpectrum(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_RESPONSE_SPECTRUM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainResponseSpectrum

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
          * - :py:attr:`~lctyp`
            - Get or set the Load curve type for defining the input spectrum.
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom for excitation input:
          * - :py:attr:`~lc_tbid`
            - Get or set the Load curve or table ID, see *DEFINE_TABLE, defining the response spectrum for frequencies. If the table definition is used a family of curves are defined for discrete critical damping ratios.
          * - :py:attr:`~sf`
            - Get or set the Scale factor for the input load spectrum.
          * - :py:attr:`~vid`
            - Get or set the Vector ID for DOF values of 4.
          * - :py:attr:`~lnid`
            - Get or set the Node ID, or node set ID, or segment set ID where the excitation is applied. If the input load is given as base excitation spectrum, LNID=0
          * - :py:attr:`~lntyp`
            - Get or set the Set type for LNID:
          * - :py:attr:`~inflag`
            - Get or set the Frequency interpolation option


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

    from frequency_domain_response_spectrum import FrequencyDomainResponseSpectrum

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

.. py:property:: lctyp
   :type: int


   
   Get or set the Load curve type for defining the input spectrum.
   EQ.0: base velocity (vs. natural frequency),
   EQ.1: base acceleration (vs. natural frequency),
   EQ.2: base displacement (vs. natural frequency),
   EQ.3: nodal force (vs. natural frequency),
   EQ.4: pressure (vs. natural frequency),
   EQ.5: base velocity (vs. natural period),
   EQ.6: base acceleration (vs. natural period),
   EQ.7: base displacement (vs. natural period),
   EQ.8: nodal force (vs. natural period),
   EQ.9: pressure (vs. natural period),
   EQ.10: base velocity time history,
   EQ.11: base acceleration time history,
   EQ.12: base displacement time history.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom for excitation input:
   EQ. 1: x-translational degree-of-freedom,
   EQ. 2: y-translational degree-of-freedom,
   EQ. 3: z-translational degree-of-freedom,
   EQ. 4: translational movement in direction given by vector VID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc_tbid
   :type: Optional[int]


   
   Get or set the Load curve or table ID, see *DEFINE_TABLE, defining the response spectrum for frequencies. If the table definition is used a family of curves are defined for discrete critical damping ratios.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor for the input load spectrum.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID for DOF values of 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: lnid
   :type: Optional[int]


   
   Get or set the Node ID, or node set ID, or segment set ID where the excitation is applied. If the input load is given as base excitation spectrum, LNID=0
















   ..
       !! processed by numpydoc !!

.. py:property:: lntyp
   :type: int


   
   Get or set the Set type for LNID:
   EQ.1: Node, see *NODE,
   EQ.2: Node set, see *SET_NODE,
   EQ.3: Segment set, see *SET_SEGMENT,
   EQ.4: Part, see *PART,
   EQ.5: Part set, see *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: inflag
   :type: int


   
   Get or set the Frequency interpolation option
   EQ.0: Logarithmic interpolation,
   EQ.1: Semi-logarithmic interpolation.
   EQ.2: Linear interpolation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_RESPONSE_SPECTRUM'






