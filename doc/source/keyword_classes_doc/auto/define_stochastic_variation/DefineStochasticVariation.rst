





:class:`DefineStochasticVariation`
==================================


.. py:class:: define_stochastic_variation.DefineStochasticVariation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_STOCHASTIC_VARIATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineStochasticVariation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id_sv`
            - Get or set the Stochastic variation ID. A unique ID number must be used
          * - :py:attr:`~pid`
            - Get or set the *PART ID or *SET_PART ID
          * - :py:attr:`~pid_typ`
            - Get or set the Flag for PID type. If PID and PID_TYP are both 0, then the
          * - :py:attr:`~icor`
            - Get or set the Correlation between the yield stress and failure strain scaling.
          * - :py:attr:`~var_s`
            - Get or set the Variation type for scaling the yield stress.
          * - :py:attr:`~var_f`
            - Get or set the Variation type for scaling failure strain.
          * - :py:attr:`~irng`
            - Get or set the Flag for random number generation.
          * - :py:attr:`~r1`
            - Get or set the Real values to define the stochastic distribution
          * - :py:attr:`~r2`
            - Get or set the Real values to define the stochastic distribution
          * - :py:attr:`~r3`
            - Get or set the Real values to define the stochastic distribution
          * - :py:attr:`~lcid`
            - Get or set the Curve ID defining the stochastic distribution
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

    from define_stochastic_variation import DefineStochasticVariation

Property detail
---------------

.. py:property:: id_sv
   :type: Optional[int]


   
   Get or set the Stochastic variation ID. A unique ID number must be used
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the *PART ID or *SET_PART ID
















   ..
       !! processed by numpydoc !!

.. py:property:: pid_typ
   :type: int


   
   Get or set the Flag for PID type. If PID and PID_TYP are both 0, then the
   properties defined here apply to all shell and solid parts using
   materials with the STOCHASTIC option.
   EQ.0: PID is a *PART ID.
   EQ.1: PID is a *SET_PART ID
















   ..
       !! processed by numpydoc !!

.. py:property:: icor
   :type: int


   
   Get or set the Correlation between the yield stress and failure strain scaling.
   EQ.0: Perfect correlation.
   EQ.1: No correlation. The yield stress and failure strain are independently scaled.
















   ..
       !! processed by numpydoc !!

.. py:property:: var_s
   :type: int


   
   Get or set the Variation type for scaling the yield stress.
   EQ.0: The scale factor is 1.0 everywhere.
   EQ.1: The scale factor is random number in the uniform random distribution in the interval defined by R1 and R2.
   EQ.2: The scale factor is a random number obeying the Gaussian distribution defined by R1, R2, and R3.
   EQ.3: The scale factor is defined by the probability distribution function defined by curve LCID.
   EQ.4: The scale factor is defined by the cumulative distribution function defined by curve LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: var_f
   :type: int


   
   Get or set the Variation type for scaling failure strain.
   EQ.0: The scale factor is 1.0 everywhere.
   EQ.1: The scale factor is random number in the uniform random distribution in the interval defined by R1 and R2.
   EQ.2: The scale factor is a random number obeying the Gaussian distribution defined by R1, R2, and R3.
   EQ.3: The scale factor is defined by the probability distribution function defined by curve LCID.
   EQ.4: The scale factor is defined by the cumulative distribution function defined by curve LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: irng
   :type: int


   
   Get or set the Flag for random number generation.
   EQ.0:   Use deterministic(pseudo - ) random number generator.The same input always leads to the same distribution.
   EQ.1 : Use non - deterministic(true) random number generator.With the same input, a different distribution is achieved in each run
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: Optional[float]


   
   Get or set the Real values to define the stochastic distribution
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: Optional[float]


   
   Get or set the Real values to define the stochastic distribution
















   ..
       !! processed by numpydoc !!

.. py:property:: r3
   :type: Optional[float]


   
   Get or set the Real values to define the stochastic distribution
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Curve ID defining the stochastic distribution
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'STOCHASTIC_VARIATION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





