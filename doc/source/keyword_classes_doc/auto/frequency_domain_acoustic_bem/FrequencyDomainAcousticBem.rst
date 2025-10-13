





:class:`FrequencyDomainAcousticBem`
===================================


.. py:class:: frequency_domain_acoustic_bem.FrequencyDomainAcousticBem(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_ACOUSTIC_BEM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainAcousticBem

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ro`
            - Get or set the Fluid Density.
          * - :py:attr:`~c`
            - Get or set the Sound speed of the fluid.
          * - :py:attr:`~fmin`
            - Get or set the Minimum value of output frequencies.
          * - :py:attr:`~fmax`
            - Get or set the Maximum value of output frequencies.
          * - :py:attr:`~nfreq`
            - Get or set the Number of output frequencies.
          * - :py:attr:`~dtout`
            - Get or set the Time interval between writing velocity or acceleration, and pressure at boundary
          * - :py:attr:`~tstart`
            - Get or set the Start time for recording velocity or acceleration in LS-DYNA simulation.
          * - :py:attr:`~pref`
            - Get or set the Reference pressure to be used to output pressure in dB, in file Press_dB. If
          * - :py:attr:`~nsidext`
            - Get or set the Set ID, or Segment set ID of output exterior field points.
          * - :py:attr:`~typext`
            - Get or set the Output exterior field point type.
          * - :py:attr:`~nsidint`
            - Get or set the Node set ID, or Segment set ID of output interior field points.
          * - :py:attr:`~typint`
            - Get or set the Output interior field point type.
          * - :py:attr:`~fftwin`
            - Get or set the FFT windows (Default=0).
          * - :py:attr:`~trslt`
            - Get or set the EQ.0: No time domain results are requested;
          * - :py:attr:`~ipfile`
            - Get or set the Flag for output files (Default=0).
          * - :py:attr:`~iunits`
            - Get or set the Flag for unit changes
          * - :py:attr:`~method`
            - Get or set the Method used in acoustic analysis (Default =0)
          * - :py:attr:`~maxit`
            - Get or set the Maximum number of iterations for iterative solver (Default =100) (Used only if METHOD>=2).
          * - :py:attr:`~tolitr`
            - Get or set the Tolerance for iterative solver (Default=1.E-4).
          * - :py:attr:`~ndd`
            - Get or set the Number of Domain Decomposition, used for memory saving.
          * - :py:attr:`~tollr`
            - Get or set the Tolerance for low rank approximation of dense matrix (Default=1.E-6).
          * - :py:attr:`~tolfct`
            - Get or set the Tolerance in factorization of low rank matrix (Default=1.E-6).
          * - :py:attr:`~ibdim`
            - Get or set the Inner iteration limit in GMRES iterative solver (Default=1000).
          * - :py:attr:`~npg`
            - Get or set the Number of Gauss integration points (Default=2).
          * - :py:attr:`~nbc`
            - Get or set the Number of boundary condition cards (Card 5) (default = 1).
          * - :py:attr:`~restrt`
            - Get or set the This flag is used to save an LS-DYNA analysis if the binary output file in the (bem=filename) option has not been changed(default = 0).
          * - :py:attr:`~iedge`
            - Get or set the Free edge and multi-connection constraints option (default = 0).
          * - :py:attr:`~noel`
            - Get or set the Location where normal velocity or acceleration is taken (default = 0).
          * - :py:attr:`~nfrup`
            - Get or set the Preconditioner update option.
          * - :py:attr:`~velout`
            - Get or set the Flag for writing out nodal or elemental velocity data.
          * - :py:attr:`~dba`
            - Get or set the Flag for writing out weighted SPL file Press_dBA with different weighting options.
          * - :py:attr:`~ssid`
            - Get or set the Part, part set ID, or segment set ID of boundary elements.
          * - :py:attr:`~sstype`
            - Get or set the Boundary element type:
          * - :py:attr:`~norm`
            - Get or set the NORM should be set such that the normal vectors point away from the fluid.
          * - :py:attr:`~bemtype`
            - Get or set the Type of input boundary values in BEM analysis.
          * - :py:attr:`~lc1`
            - Get or set the Load curve ID for defining real part of pressure, normal velocity or impedance.
          * - :py:attr:`~lc2`
            - Get or set the Load curve ID for defining imaginary part of pressure, normal velocity or impedance.
          * - :py:attr:`~t_hold`
            - Get or set the Hold-off period before the exponential window. The length of the hold-off period should coincide with the pre-trigger time to reduce the effects of noise in the captured time domain data. It is only used when FFTWIN = 5.
          * - :py:attr:`~decay`
            - Get or set the Decay ratio at the end of capture duration. For example, if the DECAY = 0.02, it means that the vibration is forced to decay to 2% of its amplitude within the capture duration. This field is only used when FFTWIN = 5.


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

    from frequency_domain_acoustic_bem import FrequencyDomainAcousticBem

Property detail
---------------

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Fluid Density.
















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


   
   Get or set the Time interval between writing velocity or acceleration, and pressure at boundary
   elements in the binary file, to be proceeded at the end of LS-DYNA simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tstart
   :type: float


   
   Get or set the Start time for recording velocity or acceleration in LS-DYNA simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: pref
   :type: float


   
   Get or set the Reference pressure to be used to output pressure in dB, in file Press_dB. If
   Ref_Pres=0, Press_dB file will not be generated. A file called Press_Pa is
   generated and contains the pressure at output nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidext
   :type: int


   
   Get or set the Set ID, or Segment set ID of output exterior field points.
















   ..
       !! processed by numpydoc !!

.. py:property:: typext
   :type: int


   
   Get or set the Output exterior field point type.
   EQ.0: node ID.
   EQ.1: Node set ID.
   EQ.2: Segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsidint
   :type: int


   
   Get or set the Node set ID, or Segment set ID of output interior field points.
















   ..
       !! processed by numpydoc !!

.. py:property:: typint
   :type: int


   
   Get or set the Output interior field point type.
   EQ.0: node ID.
   EQ.1: Node set ID.
   EQ.2: Segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: fftwin
   :type: int


   
   Get or set the FFT windows (Default=0).
   EQ.0: Rectangular window
   EQ.1: Hanning window
   EQ.2: Hamming window
   EQ.3: Blackman window
   EQ.4: Raised cosine window
   EQ.5: Exponential window.
















   ..
       !! processed by numpydoc !!

.. py:property:: trslt
   :type: int


   
   Get or set the EQ.0: No time domain results are requested;
   EQ.1: Time domain results are requested.
   EQ.2: time domain results are requested (Press_Pa_t gives real value pressure vs. time).
















   ..
       !! processed by numpydoc !!

.. py:property:: ipfile
   :type: int


   
   Get or set the Flag for output files (Default=0).
   EQ.0: Press_Pa (magnitude of pressure vs. frequency), Press_dB (sound
   pressure level vs. frequency) and bepres (ASCII database file for LSPrepost)
   are provided.
   EQ.1: Press_Pa_real (real part of the pressure vs. frequency) and
   Press_Pa_imag (imaginary part of the pressure vs. frequency) are
   included, in addition to Press_Pa, Press_dB and bepres.
   EQ.10: files for IPFILE = 0, and fringe files for acoustic pressure.
   EQ.11: files for IPFILE = 1, and fringe files for acoustic pressure.
   EQ.20: files for IPFILE = 0, and fringe files for sound pressure level.
   EQ.21: files for IPFILE = 1, and fringe files for sound pressure level.
   EQ.31: files for IPFILE = 1, and fringe files for acoustic pressure(real part).
   EQ.41: files for IPFILE = 1, and fringe files for acoustic pressure(imaginary part).
















   ..
       !! processed by numpydoc !!

.. py:property:: iunits
   :type: int


   
   Get or set the Flag for unit changes
   EQ.0: No unit change applied;
   EQ.1: MKS units are used, no change needed;
   EQ.2: Units (lbfxs2/in, inch, s, lbf, psi, etc.) are used, changed to MKS
   in BEM Acoustic computation;
   EQ.3: Units (kg, mm, ms, kN, GPa, etc.) are used, changed to MKS in
   BEM Acoustic computation;
   EQ.4: Units (ton, mm, s, N, MPa, etc.) are used, changed to MKS in
   BEM Acoustic computation.
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: int


   
   Get or set the Method used in acoustic analysis (Default =0)
   EQ.0: Rayleigh method (very fast)
   EQ.1: Kirchhoff method coupled to FEM for acoustics
   (*MAT_ACOUSTIC) (see Remark 4)
   EQ.2: Variational Indirect BEM
   EQ.3: Collocation BEM
   EQ.4: Collocation BEM with Burton-Miller formulation for exterior
   problems (no irregular frequency phenomenon).
















   ..
       !! processed by numpydoc !!

.. py:property:: maxit
   :type: int


   
   Get or set the Maximum number of iterations for iterative solver (Default =100) (Used only if METHOD>=2).
















   ..
       !! processed by numpydoc !!

.. py:property:: tolitr
   :type: float


   
   Get or set the Tolerance for iterative solver (Default=1.E-4).
















   ..
       !! processed by numpydoc !!

.. py:property:: ndd
   :type: int


   
   Get or set the Number of Domain Decomposition, used for memory saving.
   For large problems, the boundary mesh is decomposed into NDD
   domains for less memory allocation.
   This option is only used if METHOD>=2..
















   ..
       !! processed by numpydoc !!

.. py:property:: tollr
   :type: float


   
   Get or set the Tolerance for low rank approximation of dense matrix (Default=1.E-6).
















   ..
       !! processed by numpydoc !!

.. py:property:: tolfct
   :type: float


   
   Get or set the Tolerance in factorization of low rank matrix (Default=1.E-6).
















   ..
       !! processed by numpydoc !!

.. py:property:: ibdim
   :type: int


   
   Get or set the Inner iteration limit in GMRES iterative solver (Default=1000).
















   ..
       !! processed by numpydoc !!

.. py:property:: npg
   :type: int


   
   Get or set the Number of Gauss integration points (Default=2).
















   ..
       !! processed by numpydoc !!

.. py:property:: nbc
   :type: int


   
   Get or set the Number of boundary condition cards (Card 5) (default = 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: restrt
   :type: int


   
   Get or set the This flag is used to save an LS-DYNA analysis if the binary output file in the (bem=filename) option has not been changed(default = 0).
   EQ.0: LS-DYNA time domain analysis is processed and generates a new binary file.
   EQ.1: LS-DYNA time domain analysis is not processed.The binary files from previous run are used. The files include the binary output file filename, and the binary file bin_velfreq, which saves the boundary velocity from FFT.
   EQ.2: LS-DYNA restarts from d3dump file by using "R="command line parameter. This is useful when the last run was interrupted by sense switches such as "sw1".
   EQ.3: LS-DYNA reads in user provided velocity history saved in an ASCII file, bevel.
   EQ.-3:  LS-DYNA reads in user provided velocity spectrum saved in an ASCII file, bevelf
   EQ.4: run acoustic computation on a boundary element mesh with velocity information given with a denser finite element mesh in last run. This option requires both "bem = filename" and "lbem = filename2" in the command line, where filename2 is the name of the binary file generated in the last run with denser mesh.
   EQ.5: LS-DYNA time domain analysis is not processed. The binary file filename from previous run is used. An FFT is performed to get the new frequency domain boundary velocity and the results are saved in bin_velfreq.
















   ..
       !! processed by numpydoc !!

.. py:property:: iedge
   :type: int


   
   Get or set the Free edge and multi-connection constraints option (default = 0).
   EQ.0: free edge and multi-connection constraints not considered.
   EQ.1: free edge and multi-connection constraints considered.
   EQ.2: only free edge constraints are considered.
   EQ.3: only multi-connection constraints are considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: noel
   :type: int


   
   Get or set the Location where normal velocity or acceleration is taken (default = 0).
   EQ.0: elements or segments.
   EQ.1: nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfrup
   :type: int


   
   Get or set the Preconditioner update option.
   EQ.0: updated at every frequency.
   EQ.N: updated for every N frequencies.
















   ..
       !! processed by numpydoc !!

.. py:property:: velout
   :type: int


   
   Get or set the Flag for writing out nodal or elemental velocity data.
   EQ.0: No writing out velocity data.
   EQ.1: write out time domain velocity data (in x, y and z directions).
   EQ.2: write out frequency domain velocity data (in normal direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: dba
   :type: int


   
   Get or set the Flag for writing out weighted SPL file Press_dBA with different weighting options.
   EQ.0: No writing out Press_dBA.
   EQ.1: write out Press_dBA and use A-weighting.
   EQ.2: write out Press_dBA and use B-weighting.
   EQ.3: write out Press_dBA and use C-weighting.
   EQ.4: write out Press_dBA and use D-weighting.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: int


   
   Get or set the Part, part set ID, or segment set ID of boundary elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: sstype
   :type: int


   
   Get or set the Boundary element type:
   EQ.0: part Set ID
   EQ.1: part ID
   EQ.2: segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: norm
   :type: int


   
   Get or set the NORM should be set such that the normal vectors point away from the fluid.
   EQ.0: normal vectors are not inverted (default).
   EQ.1: normal vectors are inverted.
















   ..
       !! processed by numpydoc !!

.. py:property:: bemtype
   :type: int


   
   Get or set the Type of input boundary values in BEM analysis.
   EQ.0: boundary velocity will be processed in BEM analysis.
   EQ.1: boundary acceleration will be processed in BEM analysis.
   EQ.2: pressure is prescribed and the real and imaginary parts are given by LC1 and LC2.
   EQ.3: normal velocity is prescribed and the real and imaginary parts are given by LC1 and LC2.
   EQ.4: impedance is prescribed and the real and imaginary parts are given by LC1 and LC2.
   EQ.-n: normal velocity (only real part) is prescribed, through load
   curve n. An amplitude versus. frequency load curve (with curve ID n) needs to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc1
   :type: Optional[int]


   
   Get or set the Load curve ID for defining real part of pressure, normal velocity or impedance.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc2
   :type: Optional[int]


   
   Get or set the Load curve ID for defining imaginary part of pressure, normal velocity or impedance.
















   ..
       !! processed by numpydoc !!

.. py:property:: t_hold
   :type: float


   
   Get or set the Hold-off period before the exponential window. The length of the hold-off period should coincide with the pre-trigger time to reduce the effects of noise in the captured time domain data. It is only used when FFTWIN = 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: decay
   :type: float


   
   Get or set the Decay ratio at the end of capture duration. For example, if the DECAY = 0.02, it means that the vibration is forced to decay to 2% of its amplitude within the capture duration. This field is only used when FFTWIN = 5.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_ACOUSTIC_BEM'






