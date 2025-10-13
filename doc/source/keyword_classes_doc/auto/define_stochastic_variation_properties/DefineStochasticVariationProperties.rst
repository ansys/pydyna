





:class:`DefineStochasticVariationProperties`
============================================


.. py:class:: define_stochastic_variation_properties.DefineStochasticVariationProperties(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_STOCHASTIC_VARIATION_PROPERTIES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineStochasticVariationProperties

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id_sv`
            - Get or set the Stochastic variation ID. A unique ID number must be used
          * - :py:attr:`~mtype`
            - Get or set the Material type.  The available types are 10, 15, 24, 81, 98, and 213.  This variation only works for this material.
          * - :py:attr:`~pid`
            - Get or set the *PART ID or *SET_PART ID
          * - :py:attr:`~pid_typ`
            - Get or set the Flag for PID type. If PID and PID_TYP are both 0, then the
          * - :py:attr:`~irng`
            - Get or set the
          * - :py:attr:`~numv`
            - Get or set the Number of variations for a user material
          * - :py:attr:`~num_beg`
            - Get or set the The location of the first variation in the history variables for a user material. The remaining variations are added sequentially
          * - :py:attr:`~vartyp`
            - Get or set the Variation type for scaling the material property:
          * - :py:attr:`~corlgr`
            - Get or set the Correlation group number. If CORLGRP is 0, then the random number for the distribution is uncorrelated with all the other distributions. The same random number is used for evaluating all the distributions having the same positive integer value for CORLGRP
          * - :py:attr:`~r1`
            - Get or set the Real values to define the stochastic distribution
          * - :py:attr:`~r2`
            - Get or set the Real values to define the stochastic distribution
          * - :py:attr:`~r3`
            - Get or set the Real values to define the stochastic distribution
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

    from define_stochastic_variation_properties import DefineStochasticVariationProperties

Property detail
---------------

.. py:property:: id_sv
   :type: Optional[int]


   
   Get or set the Stochastic variation ID. A unique ID number must be used
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: Optional[int]


   
   Get or set the Material type.  The available types are 10, 15, 24, 81, 98, and 213.  This variation only works for this material.
















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

.. py:property:: irng
   :type: int


   
   Get or set the 
   Flag for random number generation.
   EQ.0:   Use deterministic(pseudo - ) random number generator.The same input always leads to the same distribution.
   EQ.1 : Use non - deterministic(true) random number generator.With the same input, a different distribution is achieved in each run
















   ..
       !! processed by numpydoc !!

.. py:property:: numv
   :type: int


   
   Get or set the Number of variations for a user material
















   ..
       !! processed by numpydoc !!

.. py:property:: num_beg
   :type: int


   
   Get or set the The location of the first variation in the history variables for a user material. The remaining variations are added sequentially
















   ..
       !! processed by numpydoc !!

.. py:property:: vartyp
   :type: int


   
   Get or set the Variation type for scaling the material property:
   EQ.0:   The scale factor is 1.0 everywhere.
   EQ.1 : The scale factor is a random number in the uniform random distribution in the interval defined by R1 and R2.
   EQ.2 : The scale factor is a random number obeying the Gaussian distribution defined by R1, R2,and R3.
   EQ.3 : The scale factor is defined by the probability distribution function defined by curve LCID.
   EQ.4 : The scale factor is defined by the cumulative distribution function defined by curve LCID.
















   ..
       !! processed by numpydoc !!

.. py:property:: corlgr
   :type: Optional[int]


   
   Get or set the Correlation group number. If CORLGRP is 0, then the random number for the distribution is uncorrelated with all the other distributions. The same random number is used for evaluating all the distributions having the same positive integer value for CORLGRP
















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
   :value: 'STOCHASTIC_VARIATION_PROPERTIES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





