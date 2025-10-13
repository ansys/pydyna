





:class:`MatAdhesiveCuringViscoelastic`
======================================


.. py:class:: mat_adhesive_curing_viscoelastic.MatAdhesiveCuringViscoelastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADHESIVE_CURING_VISCOELASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAdhesiveCuringViscoelastic

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
          * - :py:attr:`~k1`
            - Get or set the Parameter K1 for Kamal model.
          * - :py:attr:`~k2`
            - Get or set the Parameter K2 for Kamal model.
          * - :py:attr:`~c1`
            - Get or set the Parameter C1 for Kamal model.
          * - :py:attr:`~c2`
            - Get or set the Parameter C2 for Kamal model.
          * - :py:attr:`~m`
            - Get or set the Exponent m for Kamal model.
          * - :py:attr:`~n`
            - Get or set the Exponent n for Kamal model.
          * - :py:attr:`~chexp1`
            - Get or set the Quadratic parameter 𝛾2 for chemical shrinkage.
          * - :py:attr:`~chexp2`
            - Get or set the Linear parameter 𝛾1 for chemical shrinkage.
          * - :py:attr:`~chexp3`
            - Get or set the Constant parameter 𝛾0 for chemical shrinkage.
          * - :py:attr:`~lcchexp`
            - Get or set the |LCCHEXP| is Load curve ID to define the coefficient for chemical shrinkage 𝛾(𝛼)
          * - :py:attr:`~lcthexp`
            - Get or set the |LCTHEXP| is Load curve ID or table ID defining the coefficient of
          * - :py:attr:`~r`
            - Get or set the Gas constant 𝑅 for Kamal model.
          * - :py:attr:`~trefexp`
            - Get or set the Reference temperature 𝑇0 for secant form of thermal expansion. See Remarks below.
          * - :py:attr:`~docrefexp`
            - Get or set the Reference degree of cure 𝛼0 for sequential form of chemical      expansion. See Remarks below.
          * - :py:attr:`~wlftref`
            - Get or set the Reference temperature for WLF shift function.
          * - :py:attr:`~wlfa`
            - Get or set the Parameter A for WLF shift function.
          * - :py:attr:`~wlfb`
            - Get or set the Parameter B for WLF shift function.
          * - :py:attr:`~lcg0`
            - Get or set the Load curve ID defining the instantaneous shear modulus G0 as a function of state of cure.
          * - :py:attr:`~lck0`
            - Get or set the Load curve ID defining the instantaneous bulk modulus K0 as a function of state of cure.
          * - :py:attr:`~idoc`
            - Get or set the Initial degree of cure.
          * - :py:attr:`~incr`
            - Get or set the Switch between incremental and total stress formulation.
          * - :py:attr:`~qcure`
            - Get or set the Heat generation factor, relating the heat generated in one time step with the increment of the degree of cure in that step
          * - :py:attr:`~gi`
            - Get or set the Shear relaxation modulus for the ith term for fully cured material.
          * - :py:attr:`~betagi`
            - Get or set the Shear decay constant for the ith term for fully cured material.
          * - :py:attr:`~ki`
            - Get or set the Bulk relaxation modulus for the ith term for fully cured material.
          * - :py:attr:`~betaki`
            - Get or set the Bulk decay constant for the ith term for fully cured material.
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

    from mat_adhesive_curing_viscoelastic import MatAdhesiveCuringViscoelastic

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

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the Parameter K1 for Kamal model.
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the Parameter K2 for Kamal model.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Parameter C1 for Kamal model.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Parameter C2 for Kamal model.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Exponent m for Kamal model.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Exponent n for Kamal model.
















   ..
       !! processed by numpydoc !!

.. py:property:: chexp1
   :type: Optional[float]


   
   Get or set the Quadratic parameter 𝛾2 for chemical shrinkage.
















   ..
       !! processed by numpydoc !!

.. py:property:: chexp2
   :type: Optional[float]


   
   Get or set the Linear parameter 𝛾1 for chemical shrinkage.
















   ..
       !! processed by numpydoc !!

.. py:property:: chexp3
   :type: Optional[float]


   
   Get or set the Constant parameter 𝛾0 for chemical shrinkage.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcchexp
   :type: Optional[int]


   
   Get or set the |LCCHEXP| is Load curve ID to define the coefficient for chemical shrinkage 𝛾(𝛼)
   as a function of the state of cure 𝛼. If set, parameters CHEXP1,
   CHEXP2 and CHEXP3 are ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcthexp
   :type: Optional[int]


   
   Get or set the |LCTHEXP| is Load curve ID or table ID defining the coefficient of
   thermal expansion 𝛽(𝛼, 𝑇) as a function of cure 𝛼 and temperature 𝑇.
   If referring to a load curve, parameter 𝛽(𝑇) is a function of
   temperature 𝑇.
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Gas constant 𝑅 for Kamal model.
















   ..
       !! processed by numpydoc !!

.. py:property:: trefexp
   :type: Optional[float]


   
   Get or set the Reference temperature 𝑇0 for secant form of thermal expansion. See Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: docrefexp
   :type: Optional[float]


   
   Get or set the Reference degree of cure 𝛼0 for sequential form of chemical      expansion. See Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: wlftref
   :type: Optional[float]


   
   Get or set the Reference temperature for WLF shift function.
















   ..
       !! processed by numpydoc !!

.. py:property:: wlfa
   :type: Optional[float]


   
   Get or set the Parameter A for WLF shift function.
















   ..
       !! processed by numpydoc !!

.. py:property:: wlfb
   :type: Optional[float]


   
   Get or set the Parameter B for WLF shift function.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcg0
   :type: Optional[int]


   
   Get or set the Load curve ID defining the instantaneous shear modulus G0 as a function of state of cure.
















   ..
       !! processed by numpydoc !!

.. py:property:: lck0
   :type: Optional[int]


   
   Get or set the Load curve ID defining the instantaneous bulk modulus K0 as a function of state of cure.
















   ..
       !! processed by numpydoc !!

.. py:property:: idoc
   :type: Optional[float]


   
   Get or set the Initial degree of cure.
















   ..
       !! processed by numpydoc !!

.. py:property:: incr
   :type: int


   
   Get or set the Switch between incremental and total stress formulation.
   EQ.0: total form: (DEFAULT)
   EQ.1: incremental form: (recommended).
















   ..
       !! processed by numpydoc !!

.. py:property:: qcure
   :type: Optional[float]


   
   Get or set the Heat generation factor, relating the heat generated in one time step with the increment of the degree of cure in that step
















   ..
       !! processed by numpydoc !!

.. py:property:: gi
   :type: Optional[float]


   
   Get or set the Shear relaxation modulus for the ith term for fully cured material.
















   ..
       !! processed by numpydoc !!

.. py:property:: betagi
   :type: Optional[float]


   
   Get or set the Shear decay constant for the ith term for fully cured material.
















   ..
       !! processed by numpydoc !!

.. py:property:: ki
   :type: Optional[float]


   
   Get or set the Bulk relaxation modulus for the ith term for fully cured material.
















   ..
       !! processed by numpydoc !!

.. py:property:: betaki
   :type: Optional[float]


   
   Get or set the Bulk decay constant for the ith term for fully cured material.
















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
   :value: 'ADHESIVE_CURING_VISCOELASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





