





:class:`FrequencyDomainSsdFrf`
==============================


.. py:class:: frequency_domain_ssd_frf.FrequencyDomainSsdFrf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_SSD_FRF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainSsdFrf

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
          * - :py:attr:`~restmd`
            - Get or set the Restart option.
          * - :py:attr:`~restdp`
            - Get or set the Restart option.
          * - :py:attr:`~lcflag`
            - Get or set the Load Curve definition flag.
          * - :py:attr:`~relatv`
            - Get or set the Flag for displacement, velocity and acceleration results:
          * - :py:attr:`~dampf`
            - Get or set the Modal damping coefficient, ζ.
          * - :py:attr:`~lcdam`
            - Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
          * - :py:attr:`~lctyp`
            - Get or set the Type of load curve defining modal damping coefficient
          * - :py:attr:`~dmpmas`
            - Get or set the Mass proportional damping constant α, in Rayleigh damping..
          * - :py:attr:`~dmpstf`
            - Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
          * - :py:attr:`~dmpflg`
            - Get or set the Damping flag:
          * - :py:attr:`~istress`
            - Get or set the
          * - :py:attr:`~memory`
            - Get or set the Memory flag:
          * - :py:attr:`~nerp`
            - Get or set the Number of ERP panels
          * - :py:attr:`~strtyp`
            - Get or set the Stress used in fatigue analysis:
          * - :py:attr:`~nout`
            - Get or set the Part, part set, segment set, or node set ID for response output (use with acoustic computation). See NOTYP below.
          * - :py:attr:`~notyp`
            - Get or set the Type of NOUT:
          * - :py:attr:`~nova`
            - Get or set the Response output type:
          * - :py:attr:`~nid`
            - Get or set the Node, Node set,Segment set ID for excitation input.See NTYP below.
          * - :py:attr:`~ntyp`
            - Get or set the Type of NID:
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom for excitation input(ignored if VAD=1).
          * - :py:attr:`~vad`
            - Get or set the Excitation input type:
          * - :py:attr:`~lc1`
            - Get or set the Load Curve ID defining amplitude (LCFLAG=0) or real (in-phase) part (LCFLAG=1) of load as a function of frequency
          * - :py:attr:`~lc2`
            - Get or set the Load Curve ID defining phase angle (LCFLAG=0) or imaginary (out-phase) part (LCFLAG=1) of load as a function of frequency.
          * - :py:attr:`~sf`
            - Get or set the Scale factor for the load.
          * - :py:attr:`~vid`
            - Get or set the Vector ID for DOF=4 for excitation input, see *DEFINE_VECTOR.


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

    from frequency_domain_ssd_frf import FrequencyDomainSsdFrf

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

.. py:property:: restmd
   :type: int


   
   Get or set the Restart option.
   EQ.0: A new modal analysis is performed,
   EQ.1: Restart with d3eigv,
   EQ.2: Restart with "modeshp" binary scratch file.
















   ..
       !! processed by numpydoc !!

.. py:property:: restdp
   :type: int


   
   Get or set the Restart option.
   EQ.0: A new run without dumpssd,
   EQ.1: Restart with dumpssd.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcflag
   :type: int


   
   Get or set the Load Curve definition flag.
   EQ.0: load curves are given as amplitude / phase angle,
   EQ.1: load curves are given as real / imaginary components.
















   ..
       !! processed by numpydoc !!

.. py:property:: relatv
   :type: int


   
   Get or set the Flag for displacement, velocity and acceleration results:
   EQ.0: absolute values are requested,
   EQ.1: relative values are requested (for VAD = 2, 3 and 4 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: float


   
   Get or set the Modal damping coefficient, ζ.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdam
   :type: int


   
   Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
















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


   
   Get or set the Mass proportional damping constant α, in Rayleigh damping..
















   ..
       !! processed by numpydoc !!

.. py:property:: dmpstf
   :type: float


   
   Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmpflg
   :type: int


   
   Get or set the Damping flag:
   EQ.0: use modal damping coefficient ζ,defined by DAMPF, or LCDAM, or Rayleigh damping defined by DMPMAS and DMPSTF in this card.
   EQ.1: use damping defined by *DAMPING_PART_MASS and *DAMPING_PART_STIFFNESS.
















   ..
       !! processed by numpydoc !!

.. py:property:: istress
   :type: Optional[int]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: memory
   :type: int


   
   Get or set the Memory flag:
   EQ.0: modal superposition will be performed in-core. This is helpful to speed up the simulation.
   EQ.1: modal superposition will be performed out-of-core. This is needed for some large scale problems which require huge memory (beyond the memory available).
















   ..
       !! processed by numpydoc !!

.. py:property:: nerp
   :type: int


   
   Get or set the Number of ERP panels
















   ..
       !! processed by numpydoc !!

.. py:property:: strtyp
   :type: int


   
   Get or set the Stress used in fatigue analysis:
   EQ.0: Von Mises stress,
   EQ.1: Maximum principal stress,
   EQ.2: Maximum shear stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: nout
   :type: int


   
   Get or set the Part, part set, segment set, or node set ID for response output (use with acoustic computation). See NOTYP below.
















   ..
       !! processed by numpydoc !!

.. py:property:: notyp
   :type: int


   
   Get or set the Type of NOUT:
   EQ.0: part set ID (not implemented),
   EQ.1: part ID (not implemented),
   EQ.2: segment set ID,
   EQ.3: node set ID
   EQ.-2: segment set ID which mismatches with acoustic boundary nodes. Mapping of velocity or acceleration to the acoustic boundary nodes is performed.
















   ..
       !! processed by numpydoc !!

.. py:property:: nova
   :type: int


   
   Get or set the Response output type:
   EQ.0: velocity,
   EQ.1: acceleration.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node, Node set,Segment set ID for excitation input.See NTYP below.
















   ..
       !! processed by numpydoc !!

.. py:property:: ntyp
   :type: int


   
   Get or set the Type of NID:
   EQ.0: node ID,
   EQ.1: node set ID,
   EQ.2: segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom for excitation input(ignored if VAD=1).
   EQ. 1: x-translational degree-of-freedom x-rotational degree-of-freedom (for torque excitation, VAD=8),
   EQ. 2: y-translational degree-of-freedom or y-rotational degree-of-freedom (for torque excitation, VAD=8),
   EQ. 3: z-translational degree-of-freedom or z-rotational degree-of-freedom (for torque excitation, VAD=8),
   EQ. 4: translational movement in direction given by vector VID or rotational movement with axis given by vector VID (for torque excitation, VAD=8).
















   ..
       !! processed by numpydoc !!

.. py:property:: vad
   :type: int


   
   Get or set the Excitation input type:
   EQ.0: nodal force,
   EQ.1: pressure
   EQ.2: base velocity,
   EQ.3: base acceleration,
   EQ.4: base displacement,
   EQ.5: enforced velocity by large mass method (see remark 10),
   EQ.6: enforced acceleration by large mass method (see remark 10),
   EQ.7: enforced displacement by large mass method (see remark 10).
   EQ.8: torque.
   EQ.9: base angular velocity,
   EQ.10: base angular acceleration,
   EQ.11: base angular displacement.
   EQ.12: enforced velocity (for DIRECT type keyword options only)
   EQ.13: enforced acceleration (for DIRECT type keyword options only)
   EQ.14: enforced displacement (for DIRECT type keyword options only)
















   ..
       !! processed by numpydoc !!

.. py:property:: lc1
   :type: Optional[int]


   
   Get or set the Load Curve ID defining amplitude (LCFLAG=0) or real (in-phase) part (LCFLAG=1) of load as a function of frequency
















   ..
       !! processed by numpydoc !!

.. py:property:: lc2
   :type: Optional[int]


   
   Get or set the Load Curve ID defining phase angle (LCFLAG=0) or imaginary (out-phase) part (LCFLAG=1) of load as a function of frequency.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor for the load.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the Vector ID for DOF=4 for excitation input, see *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_SSD_FRF'






