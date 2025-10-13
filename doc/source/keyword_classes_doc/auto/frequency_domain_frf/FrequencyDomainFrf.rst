





:class:`FrequencyDomainFrf`
===========================


.. py:class:: frequency_domain_frf.FrequencyDomainFrf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_FRF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainFrf

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the Node / Node set/Segment set ID for excitation input.When VAD1,the excitation type, is set to 1, which is acceleration, this field is ignored.
          * - :py:attr:`~n1typ`
            - Get or set the Type of N1:
          * - :py:attr:`~dof1`
            - Get or set the Applicable degrees-of-freedom for excitation input (ignored if VAD1 = 4):
          * - :py:attr:`~vad1`
            - Get or set the Excitation input type:
          * - :py:attr:`~vid1`
            - Get or set the Vector ID for DOF1=0 for excitation input, see *DEFINE_VECTOR.
          * - :py:attr:`~fnmax`
            - Get or set the Optional maximum natural frequency employed in FRF computation.
          * - :py:attr:`~mdmin`
            - Get or set the The first mode employed in FRF computation (optional).
          * - :py:attr:`~mdmax`
            - Get or set the The last mode employed in FRF computation (optional).It should be set as a positive integer in a restart run(RESTRT = 1or3) based        on the number of eigenmodes available in the existing d3eigv database.
          * - :py:attr:`~dampf`
            - Get or set the Modal damping coefficient ζ.
          * - :py:attr:`~lcdam`
            - Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
          * - :py:attr:`~lctyp`
            - Get or set the Type of load curve defining modal damping coefficient:
          * - :py:attr:`~dmpmas`
            - Get or set the Mass proportional damping constant α, in Rayleigh damping.
          * - :py:attr:`~dmpstf`
            - Get or set the Stiffness proportional damping constant β, in Rayleigh damping.
          * - :py:attr:`~n2`
            - Get or set the Node / Node set/Segment set ID for response output.
          * - :py:attr:`~n2typ`
            - Get or set the Type of N2:
          * - :py:attr:`~dof2`
            - Get or set the Applicable degrees-of-freedom for response output:
          * - :py:attr:`~vad2`
            - Get or set the Response output type:
          * - :py:attr:`~vid2`
            - Get or set the Vector ID for DOF2 = 0 for response direction, see *DEFINE_VECTOR.
          * - :py:attr:`~relatv`
            - Get or set the FLAG for displacement, velocity and acceleration results:
          * - :py:attr:`~fmin`
            - Get or set the Minimum frequency for FRF output (cycles/time).
          * - :py:attr:`~fmax`
            - Get or set the Maximum frequency for FRF output (cycles/time).
          * - :py:attr:`~nfreq`
            - Get or set the Number of frequencies for FRF output.
          * - :py:attr:`~fspace`
            - Get or set the Frequency spacing option for FRF output:
          * - :py:attr:`~lcfreq`
            - Get or set the Load Curve ID defining the frequencies for FRF output.
          * - :py:attr:`~restrt`
            - Get or set the Restart option:
          * - :py:attr:`~output`
            - Get or set the Output option:


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

    from frequency_domain_frf import FrequencyDomainFrf

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node / Node set/Segment set ID for excitation input.When VAD1,the excitation type, is set to 1, which is acceleration, this field is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1typ
   :type: int


   
   Get or set the Type of N1:
   EQ.0: node ID,
   EQ.1: node set ID,
   EQ.2: segment set ID.
   When VAD1, the excitation type, is set to 1, which is acceleration,this field is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof1
   :type: int


   
   Get or set the Applicable degrees-of-freedom for excitation input (ignored if VAD1 = 4):
   EQ.0: translational movement in direction given by vector VID1,
   EQ.1: x-translational degree-of-freedom,or x-rotational degree-of-freedom (for torque excitation, VAD1 = 8)
   EQ.2: y-translational degree-of-freedom,or y-rotational degree-of-freedom (for torque excitation, VAD1 = 8),
   EQ.3: z-translational degree-of-freedom,or z-rotational degree-of-freedom (for torque excitation, VAD1 = 8).
















   ..
       !! processed by numpydoc !!

.. py:property:: vad1
   :type: int


   
   Get or set the Excitation input type:
   EQ.0: base velocity,
   EQ.1: base acceleration,
   EQ.2: base displacement,
   EQ.3: nodal force,
   EQ.4: pressure.
   EQ.5: enforced velocity by large mass method.
   EQ.6: enforced acceleration by large mass method,
   EQ.7: enforced displacement by large mass method.
   EQ.8: torque.
   EQ.9: base angular velocity,
   EQ.10: base angular acceleration,
   EQ.11: base angular displacement
















   ..
       !! processed by numpydoc !!

.. py:property:: vid1
   :type: int


   
   Get or set the Vector ID for DOF1=0 for excitation input, see *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: fnmax
   :type: float


   
   Get or set the Optional maximum natural frequency employed in FRF computation.
















   ..
       !! processed by numpydoc !!

.. py:property:: mdmin
   :type: int


   
   Get or set the The first mode employed in FRF computation (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: mdmax
   :type: int


   
   Get or set the The last mode employed in FRF computation (optional).It should be set as a positive integer in a restart run(RESTRT = 1or3) based        on the number of eigenmodes available in the existing d3eigv database.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: float


   
   Get or set the Modal damping coefficient ζ.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdam
   :type: int


   
   Get or set the Load Curve ID defining mode dependent modal damping coefficient ζ.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctyp
   :type: int


   
   Get or set the Type of load curve defining modal damping coefficient:
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

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node / Node set/Segment set ID for response output.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2typ
   :type: int


   
   Get or set the Type of N2:
   EQ.0: node ID,
   EQ.1: node set ID,
   EQ.2: segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof2
   :type: int


   
   Get or set the Applicable degrees-of-freedom for response output:
   EQ.0: direction given by vector VID2,
   EQ.1: x-translational degree-of-freedom,
   EQ.2: y-translational degree-of-freedom,
   EQ.3: z-translational degree-of-freedom,
   EQ.4: x-rotational degree-of-freedom,
   EQ.5: y-rotational degree-of-freedom,
   EQ.6: z-rotational degree-of-freedom,
   EQ.7: x, y and z-translational degrees-of-freedom,
   EQ.8: x, y and z-rotational degrees-of-freedom.
















   ..
       !! processed by numpydoc !!

.. py:property:: vad2
   :type: int


   
   Get or set the Response output type:
   EQ.0: velocity,
   EQ.1: acceleration,
   EQ.2: displacement,
   EQ.3: nodal force.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid2
   :type: int


   
   Get or set the Vector ID for DOF2 = 0 for response direction, see *DEFINE_VECTOR.
















   ..
       !! processed by numpydoc !!

.. py:property:: relatv
   :type: int


   
   Get or set the FLAG for displacement, velocity and acceleration results:
   EQ.0: absolute values are requested,
   EQ.1: relative values are requested (for VAD1=0,1,2 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmin
   :type: Optional[float]


   
   Get or set the Minimum frequency for FRF output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: Optional[float]


   
   Get or set the Maximum frequency for FRF output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of frequencies for FRF output.
















   ..
       !! processed by numpydoc !!

.. py:property:: fspace
   :type: int


   
   Get or set the Frequency spacing option for FRF output:
   EQ.0: linear,
   EQ.1: logarithmic,
   EQ.2: biased.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfreq
   :type: Optional[int]


   
   Get or set the Load Curve ID defining the frequencies for FRF output.
















   ..
       !! processed by numpydoc !!

.. py:property:: restrt
   :type: int


   
   Get or set the Restart option:
   EQ.0: initial run,
   EQ.1: restart with d3eigv family files,
   EQ.2: restart with dumpfrf,
   EQ.3: restart with d3eigv family files and dumpfrf.
















   ..
       !! processed by numpydoc !!

.. py:property:: output
   :type: int


   
   Get or set the Output option:
   EQ.0: write amplitude and phase angle pairs,
   EQ.1: write real and imaginary pairs.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_FRF'






