





:class:`ControlFrequencyResponseFunction`
=========================================


.. py:class:: control_frequency_response_function.ControlFrequencyResponseFunction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FREQUENCY_RESPONSE_FUNCTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFrequencyResponseFunction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the Node / Node set/Segment set ID for excitation input
          * - :py:attr:`~n1typ`
            - Get or set the Type of N1:
          * - :py:attr:`~dof1`
            - Get or set the Applicable degrees-of-freedom for excitation input:
          * - :py:attr:`~vad1`
            - Get or set the Excitation input type:
          * - :py:attr:`~vid`
            - Get or set the vector ID for VAD1=4 for excitation input, see *DEFINE_CURVE
          * - :py:attr:`~fnmax`
            - Get or set the Optional maximum natural frequency employed in frequency response function computation
          * - :py:attr:`~mdmin`
            - Get or set the The first mode employed in frequency response function computation.This mode id is optional
          * - :py:attr:`~mdmax`
            - Get or set the The last mode employed in frequency response function computation. This mode id is optional
          * - :py:attr:`~dampf`
            - Get or set the Modal damping coefficient.
          * - :py:attr:`~lcdam`
            - Get or set the Load Curve ID defining frequency dependent modal damping coefficient.
          * - :py:attr:`~lctyp`
            - Get or set the Type of load curve defining modal damping coefficient
          * - :py:attr:`~dmpmas`
            - Get or set the Mass proportional damping constant a, in Rayleigh damping.
          * - :py:attr:`~dmpstf`
            - Get or set the Stiffness proportional damping constant b, in Rayleigh damping
          * - :py:attr:`~n2`
            - Get or set the Node / Node set/Segment set ID for response output.
          * - :py:attr:`~n2typ`
            - Get or set the Type of N1:
          * - :py:attr:`~dof2`
            - Get or set the Applicable degrees-of-freedom for response output:
          * - :py:attr:`~vad2`
            - Get or set the Response output type:
          * - :py:attr:`~fmin`
            - Get or set the Minimum frequency for FRF output (cycles/time).
          * - :py:attr:`~fmax`
            - Get or set the Maximum frequency for FRF output (cycles/time)..
          * - :py:attr:`~nfreq`
            - Get or set the Number of frequencies for FRF output.
          * - :py:attr:`~fspace`
            - Get or set the Frequency spacing option:
          * - :py:attr:`~lcfreq`
            - Get or set the Load curve ID defining the frequencies for FRF output
          * - :py:attr:`~restrt`
            - Get or set the Restart option:


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

    from control_frequency_response_function import ControlFrequencyResponseFunction

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node / Node set/Segment set ID for excitation input
















   ..
       !! processed by numpydoc !!

.. py:property:: n1typ
   :type: int


   
   Get or set the Type of N1:
   EQ.0: node ID,
   EQ.1: node set ID,
   EQ.2: segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof1
   :type: int


   
   Get or set the Applicable degrees-of-freedom for excitation input:
   EQ.1: x-translational degree-of-freedom (positive or negative),
   EQ.2: y-translational degree-of-freedom (positive or negative),
   EQ.3: z-translational degree-of-freedom (positive or negative),
   EQ.4: translational movement in direction given by vector VID
   (positive or negative).
















   ..
       !! processed by numpydoc !!

.. py:property:: vad1
   :type: int


   
   Get or set the Excitation input type:
   EQ.0: velocity,
   EQ.1: acceleration,
   EQ.2: displacement,
   EQ.3: nodal force,
   EQ.4: pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the vector ID for VAD1=4 for excitation input, see *DEFINE_CURVE
















   ..
       !! processed by numpydoc !!

.. py:property:: fnmax
   :type: float


   
   Get or set the Optional maximum natural frequency employed in frequency response function computation
















   ..
       !! processed by numpydoc !!

.. py:property:: mdmin
   :type: int


   
   Get or set the The first mode employed in frequency response function computation.This mode id is optional
















   ..
       !! processed by numpydoc !!

.. py:property:: mdmax
   :type: int


   
   Get or set the The last mode employed in frequency response function computation. This mode id is optional
















   ..
       !! processed by numpydoc !!

.. py:property:: dampf
   :type: float


   
   Get or set the Modal damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdam
   :type: int


   
   Get or set the Load Curve ID defining frequency dependent modal damping coefficient.
















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


   
   Get or set the Mass proportional damping constant a, in Rayleigh damping.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmpstf
   :type: float


   
   Get or set the Stiffness proportional damping constant b, in Rayleigh damping
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node / Node set/Segment set ID for response output.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2typ
   :type: int


   
   Get or set the Type of N1:
   EQ.0: node ID,
   EQ.1: node set ID,
   EQ.2: segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof2
   :type: int


   
   Get or set the Applicable degrees-of-freedom for response output:
   EQ.1: x-translational degree-of-freedom,
   EQ.2: y-translational degree-of-freedom,
   EQ.3: z-translational degree-of-freedom.
















   ..
       !! processed by numpydoc !!

.. py:property:: vad2
   :type: int


   
   Get or set the Response output type:
   EQ.0: velocity,
   EQ.1: acceleration,
   EQ.2: displacement,
   EQ.3: force.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmin
   :type: Optional[float]


   
   Get or set the Minimum frequency for FRF output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: Optional[float]


   
   Get or set the Maximum frequency for FRF output (cycles/time)..
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of frequencies for FRF output.
















   ..
       !! processed by numpydoc !!

.. py:property:: fspace
   :type: int


   
   Get or set the Frequency spacing option:
   EQ.0: linear,
   EQ.1: logarithmic,
   EQ.2: biased
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfreq
   :type: Optional[int]


   
   Get or set the Load curve ID defining the frequencies for FRF output
















   ..
       !! processed by numpydoc !!

.. py:property:: restrt
   :type: int


   
   Get or set the Restart option:
   EQ.0: Initial run.
   EQ.1: Restart using d3eigv family files created in last run.
   EQ.2: Adding extra modes into last FRF results.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FREQUENCY_RESPONSE_FUNCTION'






