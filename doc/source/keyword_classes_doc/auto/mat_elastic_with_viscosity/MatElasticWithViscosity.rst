





:class:`MatElasticWithViscosity`
================================


.. py:class:: mat_elastic_with_viscosity.MatElasticWithViscosity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ELASTIC_WITH_VISCOSITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatElasticWithViscosity

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
          * - :py:attr:`~v0`
            - Get or set the Constant viscosity coefficient. If V0 is defined, don't define A, B, C or the piecewise curve (card 4).
          * - :py:attr:`~a`
            - Get or set the Viscosity coefficient a. Only, if V0 is not defined.
          * - :py:attr:`~b`
            - Get or set the Viscosity coefficient b. Only, if V0 is not defined.
          * - :py:attr:`~c`
            - Get or set the Viscosity coefficient c. Only, if V0 is not defined.
          * - :py:attr:`~lcid`
            - Get or set the Load curve, see *DEFINE_CURVE, defining viscosity versus temperature (optional).
          * - :py:attr:`~pr1`
            - Get or set the Poisson's ratio for the temperatures T1.
          * - :py:attr:`~pr2`
            - Get or set the Poisson's ratio for the temperatures T2.
          * - :py:attr:`~pr3`
            - Get or set the Poisson's ratio for the temperatures T3.
          * - :py:attr:`~pr4`
            - Get or set the Poisson's ratio for the temperatures T4.
          * - :py:attr:`~pr5`
            - Get or set the Poisson's ratio for the temperatures T5.
          * - :py:attr:`~pr6`
            - Get or set the Poisson's ratio for the temperatures T6.
          * - :py:attr:`~pr7`
            - Get or set the Poisson's ratio for the temperatures T7.
          * - :py:attr:`~pr8`
            - Get or set the Poisson's ratio for the temperatures T8.
          * - :py:attr:`~t1`
            - Get or set the First temperature, define up to 8 values.
          * - :py:attr:`~t2`
            - Get or set the Second temperature.
          * - :py:attr:`~t3`
            - Get or set the Third temperature.
          * - :py:attr:`~t4`
            - Get or set the Fourth temperature.
          * - :py:attr:`~t5`
            - Get or set the Fifth temperature.
          * - :py:attr:`~t6`
            - Get or set the Sixth temperature.
          * - :py:attr:`~t7`
            - Get or set the Seventh temperature.
          * - :py:attr:`~t8`
            - Get or set the Eighth temperature.
          * - :py:attr:`~v1`
            - Get or set the Corresponding viscosity coefficient at temperature T1 (define V1 to v8 only, if not varying with temperature).
          * - :py:attr:`~v2`
            - Get or set the Corresponding viscosity coefficient at temperature T2.
          * - :py:attr:`~v3`
            - Get or set the Corresponding viscosity coefficient at temperature T3.
          * - :py:attr:`~v4`
            - Get or set the Corresponding viscosity coefficient at temperature T4.
          * - :py:attr:`~v5`
            - Get or set the Corresponding viscosity coefficient at temperature T5.
          * - :py:attr:`~v6`
            - Get or set the Corresponding viscosity coefficient at temperature T6.
          * - :py:attr:`~v7`
            - Get or set the Corresponding viscosity coefficient at temperature T7.
          * - :py:attr:`~v8`
            - Get or set the Corresponding viscosity coefficient at temperature T8.
          * - :py:attr:`~e1`
            - Get or set the Corresponding Young's modulus at temperature T1 (define E1 to E8 only, if not varying with temperature).
          * - :py:attr:`~e2`
            - Get or set the Corresponding Young's modulus at temperature T2.
          * - :py:attr:`~e3`
            - Get or set the Corresponding Young's modulus at temperature T3.
          * - :py:attr:`~e4`
            - Get or set the Corresponding Young's modulus at temperature T4.
          * - :py:attr:`~e5`
            - Get or set the Corresponding Young's modulus at temperature T5.
          * - :py:attr:`~e6`
            - Get or set the Corresponding Young's modulus at temperature T6.
          * - :py:attr:`~e7`
            - Get or set the Corresponding Young's modulus at temperature T7.
          * - :py:attr:`~e8`
            - Get or set the Corresponding Young's modulus at temperature T8.
          * - :py:attr:`~alpha1`
            - Get or set the Corresponding thermal expansion coefficient at temperature T1.
          * - :py:attr:`~alpha2`
            - Get or set the Corresponding thermal expansion coefficient at temperature T2.
          * - :py:attr:`~alpha3`
            - Get or set the Corresponding thermal expansion coefficient at temperature T3.
          * - :py:attr:`~alpha4`
            - Get or set the Corresponding thermal expansion coefficient at temperature T4.
          * - :py:attr:`~alpha5`
            - Get or set the Corresponding thermal expansion coefficient at temperature T5.
          * - :py:attr:`~alpha6`
            - Get or set the Corresponding thermal expansion coefficient at temperature T6.
          * - :py:attr:`~alpha7`
            - Get or set the Corresponding thermal expansion coefficient at temperature T7.
          * - :py:attr:`~alpha8`
            - Get or set the Corresponding thermal expansion coefficient at temperature T8.
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

    from mat_elastic_with_viscosity import MatElasticWithViscosity

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

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Constant viscosity coefficient. If V0 is defined, don't define A, B, C or the piecewise curve (card 4).
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Viscosity coefficient a. Only, if V0 is not defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Viscosity coefficient b. Only, if V0 is not defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Viscosity coefficient c. Only, if V0 is not defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: float


   
   Get or set the Load curve, see *DEFINE_CURVE, defining viscosity versus temperature (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: pr1
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T1.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr2
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr3
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T3.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr4
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T4.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr5
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T5.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr6
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T6.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr7
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T7.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr8
   :type: Optional[float]


   
   Get or set the Poisson's ratio for the temperatures T8.
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: Optional[float]


   
   Get or set the First temperature, define up to 8 values.
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: Optional[float]


   
   Get or set the Second temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: t3
   :type: Optional[float]


   
   Get or set the Third temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: t4
   :type: Optional[float]


   
   Get or set the Fourth temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: t5
   :type: Optional[float]


   
   Get or set the Fifth temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: t6
   :type: Optional[float]


   
   Get or set the Sixth temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: t7
   :type: Optional[float]


   
   Get or set the Seventh temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: t8
   :type: Optional[float]


   
   Get or set the Eighth temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T1 (define V1 to v8 only, if not varying with temperature).
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v4
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v5
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T5.
















   ..
       !! processed by numpydoc !!

.. py:property:: v6
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T6.
















   ..
       !! processed by numpydoc !!

.. py:property:: v7
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T7.
















   ..
       !! processed by numpydoc !!

.. py:property:: v8
   :type: Optional[float]


   
   Get or set the Corresponding viscosity coefficient at temperature T8.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T1 (define E1 to E8 only, if not varying with temperature).
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T2.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T3.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T4.
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T5.
















   ..
       !! processed by numpydoc !!

.. py:property:: e6
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T6.
















   ..
       !! processed by numpydoc !!

.. py:property:: e7
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T7.
















   ..
       !! processed by numpydoc !!

.. py:property:: e8
   :type: Optional[float]


   
   Get or set the Corresponding Young's modulus at temperature T8.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T1.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T2.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T3.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha4
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T4.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha5
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T5.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha6
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T6.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha7
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T7.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha8
   :type: Optional[float]


   
   Get or set the Corresponding thermal expansion coefficient at temperature T8.
















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
   :value: 'ELASTIC_WITH_VISCOSITY'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





