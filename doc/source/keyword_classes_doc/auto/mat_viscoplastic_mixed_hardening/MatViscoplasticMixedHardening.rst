





:class:`MatViscoplasticMixedHardening`
======================================


.. py:class:: mat_viscoplastic_mixed_hardening.MatViscoplasticMixedHardening(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_VISCOPLASTIC_MIXED_HARDENING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatViscoplasticMixedHardening

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID or Table ID. Load curve ID defining effective stress
          * - :py:attr:`~beta`
            - Get or set the Hardening parameter, 0 < BETA < 1.
          * - :py:attr:`~fail`
            - Get or set the Failure flag.
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

    from mat_viscoplastic_mixed_hardening import MatViscoplasticMixedHardening

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve ID or Table ID. Load curve ID defining effective stress
   versus effective plastic strain The table ID defines for each strain
   rate value a load curve ID giving the stress versus effective plastic
   strain for that rate, See Figure M24-1. The stress versus effective
   plastic strain curve for the lowest value of strain rate is used if the
   strain rate falls below the minimum value. Likewise, the stress
   versus effective plastic strain curve for the highest value of strain
   rate is used if the strain rate exceeds the maximum value. NOTE:
   The strain rate values defined in the table may be given as the
   natural logarithm of the strain rate. If the first stress-strain curve in
   the table corresponds to a negative strain rate, LS-DYNA assumes
   that the natural logarithm of the strain rate value is used. Since the
   tables are internally discretized to equally space the points, natural
   logarithms are necessary, for example, if the curves correspond to
   rates from 10.e-04 to 10.e+04.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Hardening parameter, 0 < BETA < 1.
   EQ.0.0:  Pure kinematic hardening
   EQ.1.0:  Pure isotropic hardening
   0.0 < BETA < 1.0: Mixed hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: fail
   :type: float


   
   Get or set the Failure flag.
   LT.0.0: User defined failure subroutine is called to determine failure
   EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many calculations will be saved.
   GT.0.0: Plastic strain to failure. When the plastic strain reach-esthis value, the element is deleted from the calculation.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'VISCOPLASTIC_MIXED_HARDENING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





