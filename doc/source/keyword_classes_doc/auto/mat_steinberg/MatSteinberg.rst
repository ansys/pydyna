





:class:`MatSteinberg`
=====================


.. py:class:: mat_steinberg.MatSteinberg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_STEINBERG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSteinberg

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
          * - :py:attr:`~g0`
            - Get or set the Basic shear modulus.
          * - :py:attr:`~sigo`
            - Get or set the Sigma-0, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~beta`
            - Get or set the b, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~n`
            - Get or set the n, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~gama`
            - Get or set the Initial plastic strain, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~sigm`
            - Get or set the Sigma-m, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~b`
            - Get or set the b, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~bp`
            - Get or set the b', see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~h`
            - Get or set the h, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~f`
            - Get or set the f, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~a`
            - Get or set the Atomic weight (if = 0.0, RP (R') must be defined).
          * - :py:attr:`~tmo`
            - Get or set the Tm0, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~gamo`
            - Get or set the gamma-0, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~sa`
            - Get or set the a, see defining equations in keyword manual page 51 (volume two).
          * - :py:attr:`~pc`
            - Get or set the Pcut or -sigma-f (default=-1.e+30).
          * - :py:attr:`~spall`
            - Get or set the Spall type:
          * - :py:attr:`~rp`
            - Get or set the R'. If R' not equal to 0.0, A is not defined.
          * - :py:attr:`~flag`
            - Get or set the Set to 1.0 for mu coefficients for the cold compression energy fit. Default is nu.
          * - :py:attr:`~mmn`
            - Get or set the mu-min or nu-min . Optional mu or nu minimum value.
          * - :py:attr:`~mmx`
            - Get or set the mu-max or nu-max . Optional mu or nu maximum value.
          * - :py:attr:`~eco`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec1`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec2`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec3`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec4`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec5`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec6`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec7`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec8`
            - Get or set the Cold compression energy coefficient (optional).
          * - :py:attr:`~ec9`
            - Get or set the Cold compression energy coefficient (optional).
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

    from mat_steinberg import MatSteinberg

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

.. py:property:: g0
   :type: Optional[float]


   
   Get or set the Basic shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigo
   :type: Optional[float]


   
   Get or set the Sigma-0, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the b, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the n, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: gama
   :type: Optional[float]


   
   Get or set the Initial plastic strain, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigm
   :type: Optional[float]


   
   Get or set the Sigma-m, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the b, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: bp
   :type: Optional[float]


   
   Get or set the b', see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the h, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: f
   :type: Optional[float]


   
   Get or set the f, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Atomic weight (if = 0.0, RP (R') must be defined).
















   ..
       !! processed by numpydoc !!

.. py:property:: tmo
   :type: Optional[float]


   
   Get or set the Tm0, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: gamo
   :type: Optional[float]


   
   Get or set the gamma-0, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: sa
   :type: Optional[float]


   
   Get or set the a, see defining equations in keyword manual page 51 (volume two).
















   ..
       !! processed by numpydoc !!

.. py:property:: pc
   :type: float


   
   Get or set the Pcut or -sigma-f (default=-1.e+30).
















   ..
       !! processed by numpydoc !!

.. py:property:: spall
   :type: float


   
   Get or set the Spall type:
   EQ. 0.0: default set to 2.0,
   EQ. 1.0: P => Pcut ,
   EQ. 2.0: if sigma-max => -Pcut element spalls and tension, p < 0, is never allowed,
   EQ. 3.0: P < -Pcut element spalls and tension, p < 0, is never allowed.
















   ..
       !! processed by numpydoc !!

.. py:property:: rp
   :type: Optional[float]


   
   Get or set the R'. If R' not equal to 0.0, A is not defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: flag
   :type: float


   
   Get or set the Set to 1.0 for mu coefficients for the cold compression energy fit. Default is nu.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmn
   :type: Optional[float]


   
   Get or set the mu-min or nu-min . Optional mu or nu minimum value.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmx
   :type: Optional[float]


   
   Get or set the mu-max or nu-max . Optional mu or nu maximum value.
















   ..
       !! processed by numpydoc !!

.. py:property:: eco
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec1
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec2
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec3
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec4
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec5
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec6
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec7
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec8
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec9
   :type: Optional[float]


   
   Get or set the Cold compression energy coefficient (optional).
















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
   :value: 'STEINBERG'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





