





:class:`Mat060C`
================


.. py:class:: mat_060c.Mat060C(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_060C keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat060C

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
          * - :py:attr:`~pr_lc`
            - Get or set the Load curve (see *DEFINE_CURVE) defining Poisson's ratio as a function of temperature.
          * - :py:attr:`~ym_lc`
            - Get or set the Load curve (see *DEFINE_CURVE) defining Young's modulus as a function of temperature.
          * - :py:attr:`~a_lc`
            - Get or set the Load curve (see *DEFINE_CURVE) defining the coefficient of thermal expansion as a function of temperature.
          * - :py:attr:`~v_lc`
            - Get or set the Load curve (see *DEFINE_CURVE) defining the viscosity as a function of temperature.
          * - :py:attr:`~v_log`
            - Get or set the Flag for the form of V_LC. If V_LOG=1.0, the value specified in V_LC is the natural logarithm of the viscosity, ln(V). The value interpolated from the curve is then exponentiated to obtain the viscosity. If V_LOG=0.0, the value is the viscosity. The logarithmic form is useful if the value of the viscosity changes by orders of magnitude over the temperature range of the data.
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

    from mat_060c import Mat060C

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

.. py:property:: pr_lc
   :type: Optional[int]


   
   Get or set the Load curve (see *DEFINE_CURVE) defining Poisson's ratio as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: ym_lc
   :type: Optional[int]


   
   Get or set the Load curve (see *DEFINE_CURVE) defining Young's modulus as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: a_lc
   :type: Optional[int]


   
   Get or set the Load curve (see *DEFINE_CURVE) defining the coefficient of thermal expansion as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: v_lc
   :type: Optional[int]


   
   Get or set the Load curve (see *DEFINE_CURVE) defining the viscosity as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: v_log
   :type: float


   
   Get or set the Flag for the form of V_LC. If V_LOG=1.0, the value specified in V_LC is the natural logarithm of the viscosity, ln(V). The value interpolated from the curve is then exponentiated to obtain the viscosity. If V_LOG=0.0, the value is the viscosity. The logarithmic form is useful if the value of the viscosity changes by orders of magnitude over the temperature range of the data.
















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
   :value: '060C'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





