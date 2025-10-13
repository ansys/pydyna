





:class:`MatTransverselyAnisotropicElasticPlasticEchange`
========================================================


.. py:class:: mat_transversely_anisotropic_elastic_plastic_echange.MatTransverselyAnisotropicElasticPlasticEchange(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_TRANSVERSELY_ANISOTROPIC_ELASTIC_PLASTIC_ECHANGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatTransverselyAnisotropicElasticPlasticEchange

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~sigy`
            - Get or set the Yield stress.
          * - :py:attr:`~etan`
            - Get or set the Plastic hardening modulus.
          * - :py:attr:`~r`
            - Get or set the Anisotropic hardening parameter.
          * - :py:attr:`~hlcid`
            - Get or set the Load curve ID defining effective yield stress versus effective plastic strain.
          * - :py:attr:`~idscale`
            - Get or set the load curve ID defining the scale factor for the Young's modulus change with respect to effective strain (if EA and COE are defined), this curve is not necessary).
          * - :py:attr:`~ea`
            - Get or set the coefficients defining the Young's modulus with respect to the effective strain, EA is   and Coe is  (if IDSCALE is defined, these two parameters are not necessary).
          * - :py:attr:`~coe`
            - Get or set the coefficients defining the Young's modulus with respect to the effective strain, EA is   and Coe is  (if IDSCALE is defined, these two parameters are not necessary).
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

    from mat_transversely_anisotropic_elastic_plastic_echange import MatTransverselyAnisotropicElasticPlasticEchange

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

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Plastic hardening modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Anisotropic hardening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: hlcid
   :type: int


   
   Get or set the Load curve ID defining effective yield stress versus effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: idscale
   :type: Optional[int]


   
   Get or set the load curve ID defining the scale factor for the Young's modulus change with respect to effective strain (if EA and COE are defined), this curve is not necessary).
















   ..
       !! processed by numpydoc !!

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the coefficients defining the Young's modulus with respect to the effective strain, EA is   and Coe is  (if IDSCALE is defined, these two parameters are not necessary).
















   ..
       !! processed by numpydoc !!

.. py:property:: coe
   :type: Optional[float]


   
   Get or set the coefficients defining the Young's modulus with respect to the effective strain, EA is   and Coe is  (if IDSCALE is defined, these two parameters are not necessary).
















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
   :value: 'TRANSVERSELY_ANISOTROPIC_ELASTIC_PLASTIC_ECHANGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





