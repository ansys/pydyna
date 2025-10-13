





:class:`FrequencyDomainAcousticFemEigenvalue`
=============================================


.. py:class:: frequency_domain_acoustic_fem_eigenvalue.FrequencyDomainAcousticFemEigenvalue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_ACOUSTIC_FEM_EIGENVALUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainAcousticFemEigenvalue

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ro`
            - Get or set the Fluid density.
          * - :py:attr:`~c`
            - Get or set the Sound speed of the fluid.
          * - :py:attr:`~fmin`
            - Get or set the Minimum value of output frequencies.
          * - :py:attr:`~fmax`
            - Get or set the Maximum value of output frequencies.
          * - :py:attr:`~nfreq`
            - Get or set the Number of output frequencies.
          * - :py:attr:`~dtout`
            - Get or set the Time step for writing velocity or acceleration in the binary file.
          * - :py:attr:`~tstart`
            - Get or set the Start time for recording velocity or acceleration in transient analysis.
          * - :py:attr:`~pref`
            - Get or set the Reference pressure, for converting the acoustic pressure to dB.
          * - :py:attr:`~fftwin`
            - Get or set the FFT windows (Default=0):
          * - :py:attr:`~mixdmp`
            - Get or set the Acoustic stiffness and mass matrices dumping (when using the option EIGENVALUE):
          * - :py:attr:`~pid`
            - Get or set the Part ID, or part set ID to define the acoustic domain.
          * - :py:attr:`~ptyp`
            - Get or set the Set type:
          * - :py:attr:`~sid`
            - Get or set the Part ID, or part set ID, or segment set ID, or node set ID to define the boundary where vibration boundary condition is provided
          * - :py:attr:`~styp`
            - Get or set the Set type:
          * - :py:attr:`~vad`
            - Get or set the Boundary condition flag:
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~lcid1`
            - Get or set the Load curve ID to describe the amplitude (or real part) of velocity, see *DEFINE_CURVE.
          * - :py:attr:`~lcid2`
            - Get or set the Load curve ID to describe the phase (or imaginary part) of velocity, see *DEFINE_CURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.
          * - :py:attr:`~vid`
            - Get or set the Vector ID for DOF values of 4.


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

    from frequency_domain_acoustic_fem_eigenvalue import FrequencyDomainAcousticFemEigenvalue

Property detail
---------------

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Fluid density.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Sound speed of the fluid.
   GT.0: real constant sound speed.
   LT.0: |C| is the load curve ID,which defines the frequency dependent complex sound speed.See *FREQUENCY_DOMAIN_ACOUSTIC_SOUND_SPEED.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmin
   :type: Optional[float]


   
   Get or set the Minimum value of output frequencies.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: Optional[float]


   
   Get or set the Maximum value of output frequencies.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of output frequencies.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time step for writing velocity or acceleration in the binary file.
















   ..
       !! processed by numpydoc !!

.. py:property:: tstart
   :type: float


   
   Get or set the Start time for recording velocity or acceleration in transient analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: pref
   :type: float


   
   Get or set the Reference pressure, for converting the acoustic pressure to dB.
















   ..
       !! processed by numpydoc !!

.. py:property:: fftwin
   :type: int


   
   Get or set the FFT windows (Default=0):
   EQ.0:   Rectangular window.
   EQ.1:   Hanning window.
   EQ.2:   Hamming window.
   EQ.3:   Blackman window.
   EQ.4:   Raised cosine window.
















   ..
       !! processed by numpydoc !!

.. py:property:: mixdmp
   :type: int


   
   Get or set the Acoustic stiffness and mass matrices dumping (when using the option EIGENVALUE):
   EQ.0:   no dumping.
   EQ.1:   dumping globally assembled acoustic stiffness and mass matrices in Harwell-Boeing sparse matrix format.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, or part set ID to define the acoustic domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptyp
   :type: int


   
   Get or set the Set type:
   EQ.0: part, see *PART.
   EQ.1: part set, see *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Part ID, or part set ID, or segment set ID, or node set ID to define the boundary where vibration boundary condition is provided
















   ..
       !! processed by numpydoc !!

.. py:property:: styp
   :type: int


   
   Get or set the Set type:
   EQ.0: part, see *PART.
   EQ.1: part set, see *SET_PART.
   EQ.2: segment set, see *SET_SEGMENT.
   EQ.3: node set, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: vad
   :type: int


   
   Get or set the Boundary condition flag:
   EQ.0: velocity by steady state dynamics (SSD).
   EQ.1: velocity by transient analysis.
   EQ.2: opening(zero pressure).
   EQ.11: velocity by LCID1 (amplitude) and LCID2 (phase).
   EQ.12: velocity by LCID1 (real) and LCID2 (imaginary).
   EQ.21: acceleration by LCID1 (amplitude) and LCID2 (phase).
   EQ.22: acceleration by LCID1 (real) and LCID2 (imaginary).
   EQ.31: displacement by LCID1 (amplitude) and LCID2 (phase).
   EQ.32: displacement by LCID1 (real) and LCID2 (imaginary).
   EQ.41: impedance by LCID1 (amplitude) and LCID2 (phase).
   EQ.42: impedance by LCID1 (real) and LCID2 (imaginary).
   EQ.51: pressure by LCID1 (amplitude) and LCID2 (phase).
   EQ.52: pressure by LCID1 (real) and LCID2 (imaginary).
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ.0: determined by steady state dynamics.
   EQ.1: x-translational degree-of-freedom,
   EQ.2: y-translational degree-of-freedom,
   EQ.3: z-translational degree-of-freedom,
   EQ.4: translational motion in direction given by VID,
   EQ.5: normal direction of the element or segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid1
   :type: int


   
   Get or set the Load curve ID to describe the amplitude (or real part) of velocity, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid2
   :type: int


   
   Get or set the Load curve ID to describe the phase (or imaginary part) of velocity, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the Vector ID for DOF values of 4.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_ACOUSTIC_FEM_EIGENVALUE'






