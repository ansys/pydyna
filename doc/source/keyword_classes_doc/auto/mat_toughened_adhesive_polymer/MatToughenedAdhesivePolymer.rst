





:class:`MatToughenedAdhesivePolymer`
====================================


.. py:class:: mat_toughened_adhesive_polymer.MatToughenedAdhesivePolymer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_TOUGHENED_ADHESIVE_POLYMER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatToughenedAdhesivePolymer

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
          * - :py:attr:`~flg`
            - Get or set the Flag to choose between yield functions f and f^, see Remarks.
          * - :py:attr:`~jcfl`
            - Get or set the Johnson & Cook constitutive failure criterion flag, see Remarks.
          * - :py:attr:`~dopt`
            - Get or set the Damage criterion flag D or D^, see Remarks.
          * - :py:attr:`~lcss`
            - Get or set the Curve ID or Table ID.
          * - :py:attr:`~tau0`
            - Get or set the Initial shear yield stress.
          * - :py:attr:`~q`
            - Get or set the Isotropic nonlinear hardening modulus q.
          * - :py:attr:`~b`
            - Get or set the Isotropic exponential decay parameter b.
          * - :py:attr:`~h`
            - Get or set the Isotropic linear hardening modulus H
          * - :py:attr:`~c`
            - Get or set the Strain rate coefficient C
          * - :py:attr:`~gam0`
            - Get or set the Quasi-static threshold strain rate
          * - :py:attr:`~gamm`
            - Get or set the Maximum threshold strain rate
          * - :py:attr:`~a10`
            - Get or set the Yield function parameter: initial value.
          * - :py:attr:`~a20`
            - Get or set the Yield function parameter: initial value.
          * - :py:attr:`~a1h`
            - Get or set the Yield function parameter for formative hardening.
          * - :py:attr:`~a2h`
            - Get or set the Yield function parameter for formative hardening.
          * - :py:attr:`~a2s`
            - Get or set the Plastic potential parameter for hydrostatic stress term
          * - :py:attr:`~pow`
            - Get or set the Exponent of the phenomenological damage model
          * - :py:attr:`~srfilt`
            - Get or set the Strain rate filtering parameter in exponential moving average with admissible values ranging from 0 to 1
          * - :py:attr:`~d1`
            - Get or set the Johnson & Cook failure parameter d1.
          * - :py:attr:`~d2`
            - Get or set the Johnson & Cook failure parameter d2.
          * - :py:attr:`~d3`
            - Get or set the Johnson & Cook failure parameter d3
          * - :py:attr:`~d4`
            - Get or set the Johnson & Cook rate dependent failure parameter d4.
          * - :py:attr:`~d1c`
            - Get or set the Johnson & Cook damage threshold parameter d1c
          * - :py:attr:`~d2c`
            - Get or set the Johnson & Cook damage threshold parameter d2c
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

    from mat_toughened_adhesive_polymer import MatToughenedAdhesivePolymer

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

.. py:property:: flg
   :type: int


   
   Get or set the Flag to choose between yield functions f and f^, see Remarks.
   EQ.0.0: Cap in tension and nonlinear Drucker & Prager in compression,
   EQ.2.0: Cap in tension. and von Mises in compression
















   ..
       !! processed by numpydoc !!

.. py:property:: jcfl
   :type: int


   
   Get or set the Johnson & Cook constitutive failure criterion flag, see Remarks.
   EQ.0.0: use triaxiality factor only in tension,
   EQ.1.0: use triaxiality factor in tension and compression
















   ..
       !! processed by numpydoc !!

.. py:property:: dopt
   :type: Optional[int]


   
   Get or set the Damage criterion flag D or D^, see Remarks.
   EQ.0.0: damage model uses damage plastic strain r.
   damage model uses plastic arc length rv
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Curve ID or Table ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: tau0
   :type: Optional[float]


   
   Get or set the Initial shear yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Isotropic nonlinear hardening modulus q.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Isotropic exponential decay parameter b.
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Isotropic linear hardening modulus H
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Strain rate coefficient C
















   ..
       !! processed by numpydoc !!

.. py:property:: gam0
   :type: Optional[float]


   
   Get or set the Quasi-static threshold strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: gamm
   :type: Optional[float]


   
   Get or set the Maximum threshold strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: a10
   :type: Optional[float]


   
   Get or set the Yield function parameter: initial value.
















   ..
       !! processed by numpydoc !!

.. py:property:: a20
   :type: Optional[float]


   
   Get or set the Yield function parameter: initial value.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1h
   :type: Optional[float]


   
   Get or set the Yield function parameter for formative hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2h
   :type: Optional[float]


   
   Get or set the Yield function parameter for formative hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2s
   :type: Optional[float]


   
   Get or set the Plastic potential parameter for hydrostatic stress term
















   ..
       !! processed by numpydoc !!

.. py:property:: pow
   :type: Optional[float]


   
   Get or set the Exponent of the phenomenological damage model
















   ..
       !! processed by numpydoc !!

.. py:property:: srfilt
   :type: Optional[float]


   
   Get or set the Strain rate filtering parameter in exponential moving average with admissible values ranging from 0 to 1
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Johnson & Cook failure parameter d1.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Johnson & Cook failure parameter d2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Johnson & Cook failure parameter d3
















   ..
       !! processed by numpydoc !!

.. py:property:: d4
   :type: Optional[float]


   
   Get or set the Johnson & Cook rate dependent failure parameter d4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1c
   :type: Optional[float]


   
   Get or set the Johnson & Cook damage threshold parameter d1c
















   ..
       !! processed by numpydoc !!

.. py:property:: d2c
   :type: Optional[float]


   
   Get or set the Johnson & Cook damage threshold parameter d2c
















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
   :value: 'TOUGHENED_ADHESIVE_POLYMER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





