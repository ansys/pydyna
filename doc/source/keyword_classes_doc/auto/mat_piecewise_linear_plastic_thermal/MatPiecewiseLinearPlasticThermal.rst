





:class:`MatPiecewiseLinearPlasticThermal`
=========================================


.. py:class:: mat_piecewise_linear_plastic_thermal.MatPiecewiseLinearPlasticThermal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_PIECEWISE_LINEAR_PLASTIC_THERMAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPiecewiseLinearPlasticThermal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.  A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~c`
            - Get or set the Strain rate parameter.
          * - :py:attr:`~p`
            - Get or set the Strain rate parameter.
          * - :py:attr:`~fail`
            - Get or set the Effective plastic strain when the material fails. Note that for solids the *MAT_ADD_EROSION can be used for additional failure criteria.
          * - :py:attr:`~tdel`
            - Get or set the A time step less then TDEL is not allowed. A step size less than TDEL trigger automatic element deletion. This option is ignored for implicit analyses.
          * - :py:attr:`~tabidc`
            - Get or set the Table ID for yield stress in compression.
          * - :py:attr:`~tabidt`
            - Get or set the Table ID for yield stress in tension.
          * - :py:attr:`~lalpha`
            - Get or set the Load curve ID for thermal expansion coefficient as a function of temperature.
          * - :py:attr:`~alpha`
            - Get or set the Coefficient of thermal expansion.
          * - :py:attr:`~tref`
            - Get or set the Reference temperature, which is required if and only if LALPHA is given with a negative load curve ID.
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

    from mat_piecewise_linear_plastic_thermal import MatPiecewiseLinearPlasticThermal

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.  A unique number or label must be specified.
















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

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Strain rate parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Strain rate parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: fail
   :type: Optional[float]


   
   Get or set the Effective plastic strain when the material fails. Note that for solids the *MAT_ADD_EROSION can be used for additional failure criteria.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdel
   :type: Optional[float]


   
   Get or set the A time step less then TDEL is not allowed. A step size less than TDEL trigger automatic element deletion. This option is ignored for implicit analyses.
















   ..
       !! processed by numpydoc !!

.. py:property:: tabidc
   :type: Optional[int]


   
   Get or set the Table ID for yield stress in compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: tabidt
   :type: Optional[int]


   
   Get or set the Table ID for yield stress in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: lalpha
   :type: Optional[int]


   
   Get or set the Load curve ID for thermal expansion coefficient as a function of temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Coefficient of thermal expansion.
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: Optional[float]


   
   Get or set the Reference temperature, which is required if and only if LALPHA is given with a negative load curve ID.
















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
   :value: 'PIECEWISE_LINEAR_PLASTIC_THERMAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





