





:class:`MatConcreteDamage`
==========================


.. py:class:: mat_concrete_damage.MatConcreteDamage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CONCRETE_DAMAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatConcreteDamage

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~sigf`
            - Get or set the Maximum principal stress for failure.
          * - :py:attr:`~a0`
            - Get or set the Cohesion.
          * - :py:attr:`~a1`
            - Get or set the Pressure hardening coefficient.
          * - :py:attr:`~a2`
            - Get or set the Pressure hardening coefficient.
          * - :py:attr:`~a0y`
            - Get or set the Cohesion for yield.
          * - :py:attr:`~a1y`
            - Get or set the Pressure hardening coefficient for yield limit.
          * - :py:attr:`~a2y`
            - Get or set the Pressure hardening coefficient for yield limit.
          * - :py:attr:`~a1f`
            - Get or set the Pressure hardening coefficient for failed material.
          * - :py:attr:`~a2f`
            - Get or set the Pressure hardening coefficient for failed material.
          * - :py:attr:`~b1`
            - Get or set the Damage scaling factor.
          * - :py:attr:`~b2`
            - Get or set the Damage scaling factor for uniaxial tensile path.
          * - :py:attr:`~b3`
            - Get or set the Damage scaling factor for triaxial tensile path.
          * - :py:attr:`~per`
            - Get or set the Percent reinforcement.
          * - :py:attr:`~er`
            - Get or set the Elastic modulus for reinforcement.
          * - :py:attr:`~prr`
            - Get or set the Poisson's ratio for reinforcement.
          * - :py:attr:`~sigy`
            - Get or set the Initial yield stress.
          * - :py:attr:`~etan`
            - Get or set the Tangent modulus/plastic hardening modulus.
          * - :py:attr:`~lcp`
            - Get or set the Load curve ID giving rate sensitivity for principal material, see *DEFINE_CURVE.
          * - :py:attr:`~lcr`
            - Get or set the Load curve ID giving rate sensitivity for reinforcement, see *DEFINE_CURVE.
          * - :py:attr:`~lambda_1`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_2`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_3`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_4`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_5`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_6`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_7`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_8`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_9`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_10`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_11`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_12`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~lambda_13`
            - Get or set the Tabulated damage function.
          * - :py:attr:`~nu_1`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_2`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_3`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_4`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_5`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_6`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_7`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_8`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_9`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_10`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_11`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_12`
            - Get or set the Tabulated scale factor.
          * - :py:attr:`~nu_13`
            - Get or set the Tabulated scale factor.
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

    from mat_concrete_damage import MatConcreteDamage

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigf
   :type: Optional[float]


   
   Get or set the Maximum principal stress for failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: a0
   :type: Optional[float]


   
   Get or set the Cohesion.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Pressure hardening coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Pressure hardening coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: a0y
   :type: Optional[float]


   
   Get or set the Cohesion for yield.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1y
   :type: Optional[float]


   
   Get or set the Pressure hardening coefficient for yield limit.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2y
   :type: Optional[float]


   
   Get or set the Pressure hardening coefficient for yield limit.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1f
   :type: Optional[float]


   
   Get or set the Pressure hardening coefficient for failed material.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2f
   :type: Optional[float]


   
   Get or set the Pressure hardening coefficient for failed material.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Damage scaling factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the Damage scaling factor for uniaxial tensile path.
















   ..
       !! processed by numpydoc !!

.. py:property:: b3
   :type: Optional[float]


   
   Get or set the Damage scaling factor for triaxial tensile path.
















   ..
       !! processed by numpydoc !!

.. py:property:: per
   :type: Optional[float]


   
   Get or set the Percent reinforcement.
















   ..
       !! processed by numpydoc !!

.. py:property:: er
   :type: Optional[float]


   
   Get or set the Elastic modulus for reinforcement.
















   ..
       !! processed by numpydoc !!

.. py:property:: prr
   :type: Optional[float]


   
   Get or set the Poisson's ratio for reinforcement.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Tangent modulus/plastic hardening modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcp
   :type: Optional[int]


   
   Get or set the Load curve ID giving rate sensitivity for principal material, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcr
   :type: Optional[int]


   
   Get or set the Load curve ID giving rate sensitivity for reinforcement, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_1
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_2
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_3
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_4
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_5
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_6
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_7
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_8
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_9
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_10
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_11
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_12
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_13
   :type: Optional[float]


   
   Get or set the Tabulated damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_1
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_2
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_3
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_4
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_5
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_6
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_7
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_8
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_9
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_10
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_11
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_12
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu_13
   :type: Optional[float]


   
   Get or set the Tabulated scale factor.
















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
   :value: 'CONCRETE_DAMAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





