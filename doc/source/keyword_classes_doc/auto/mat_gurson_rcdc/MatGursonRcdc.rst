





:class:`MatGursonRcdc`
======================


.. py:class:: mat_gurson_rcdc.MatGursonRcdc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_GURSON_RCDC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatGursonRcdc

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
          * - :py:attr:`~n`
            - Get or set the Exponent for Power law.This value is only used if ATYP=1 and LCSS=0.
          * - :py:attr:`~q1`
            - Get or set the Parameter q1.
          * - :py:attr:`~q2`
            - Get or set the Parameter q2.
          * - :py:attr:`~fc`
            - Get or set the Critical void volume fraction fc.
          * - :py:attr:`~f0`
            - Get or set the Initial void volume fraction f0.
          * - :py:attr:`~en`
            - Get or set the Mean nucleation strain En .
          * - :py:attr:`~sn`
            - Get or set the Standard deviation Sn of the normal distribution of En.
          * - :py:attr:`~fn`
            - Get or set the Void volume fraction of nucleating particles.
          * - :py:attr:`~etan`
            - Get or set the Hardening modulus. This value is only used if ATYP=2 and LCSS=0.
          * - :py:attr:`~atyp`
            - Get or set the Type of hardening.
          * - :py:attr:`~ff0`
            - Get or set the Failure void volume fraction. This value is used if no curve is given by the points L1,FF1 - L4,FF4 and LCLF=0.
          * - :py:attr:`~eps1`
            - Get or set the Effective plastic strain values.The first point must be zero corresponding to the initial yield stress. This option is only used if ATYP equal to 3. At least 2 points should be defined.These values are used if ATYP=3 and LCSS=0.
          * - :py:attr:`~eps2`
            - Get or set the Effective plastic strain values at point 2
          * - :py:attr:`~eps3`
            - Get or set the Effective plastic strain values at point 3
          * - :py:attr:`~eps4`
            - Get or set the Effective plastic strain values at point 4
          * - :py:attr:`~eps5`
            - Get or set the Effective plastic strain values at point 5
          * - :py:attr:`~eps6`
            - Get or set the Effective plastic strain values at point 6
          * - :py:attr:`~eps7`
            - Get or set the Effective plastic strain values at point 7
          * - :py:attr:`~eps8`
            - Get or set the Effective plastic strain values at point 8
          * - :py:attr:`~es1`
            - Get or set the Corresponding yield stress values to EPS1 - EPS8. These values are used if ATYP=3 and LCSS=0.
          * - :py:attr:`~es2`
            - Get or set the Corresponding yield stress values to EPS2
          * - :py:attr:`~es3`
            - Get or set the Corresponding yield stress values to EPS3
          * - :py:attr:`~es4`
            - Get or set the Corresponding yield stress values to EPS4
          * - :py:attr:`~es5`
            - Get or set the Corresponding yield stress values to EPS5
          * - :py:attr:`~es6`
            - Get or set the Corresponding yield stress values to EPS6
          * - :py:attr:`~es7`
            - Get or set the Corresponding yield stress values to EPS7
          * - :py:attr:`~es8`
            - Get or set the Corresponding yield stress values to EPS8
          * - :py:attr:`~l1`
            - Get or set the Element length values.These values are only used if LCLF=0.
          * - :py:attr:`~l2`
            - Get or set the Element length values.
          * - :py:attr:`~l3`
            - Get or set the Element length values.
          * - :py:attr:`~l4`
            - Get or set the Element length values.
          * - :py:attr:`~ff1`
            - Get or set the Corresponding failure void volume fraction. These values are only used if LCLF=0.
          * - :py:attr:`~ff2`
            - Get or set the Corresponding failure void volume fraction.
          * - :py:attr:`~ff3`
            - Get or set the Corresponding failure void volume fraction.
          * - :py:attr:`~ff4`
            - Get or set the Corresponding failure void volume fraction.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID defining effective stress versus effective plastic strain. ATYP is ignored with this option.
          * - :py:attr:`~lclf`
            - Get or set the Load curve ID defining failure void volume fraction versus element length. The values L1-L4 and FF1-FF4 are ignored with this option.
          * - :py:attr:`~numint`
            - Get or set the Number of through thickness integration points which must fail before the element is deleted.
          * - :py:attr:`~alpha`
            - Get or set the Parameter alpha. for the Rc-Dc model
          * - :py:attr:`~beta`
            - Get or set the Parameter beta. for the Rc-Dc model
          * - :py:attr:`~gamma`
            - Get or set the Parameter gamma. for the Rc-Dc model
          * - :py:attr:`~d0`
            - Get or set the Parameter D0. for the Rc-Dc model
          * - :py:attr:`~b`
            - Get or set the Parameter b. for the Rc-Dc model
          * - :py:attr:`~lambda_`
            - Get or set the Parameter lambda. for the Rc-Dc model
          * - :py:attr:`~ds`
            - Get or set the Parameter Ds. for the Rc-Dc model
          * - :py:attr:`~l`
            - Get or set the Characteristic element length for this material
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

    from mat_gurson_rcdc import MatGursonRcdc

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

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Exponent for Power law.This value is only used if ATYP=1 and LCSS=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: q1
   :type: Optional[float]


   
   Get or set the Parameter q1.
















   ..
       !! processed by numpydoc !!

.. py:property:: q2
   :type: Optional[float]


   
   Get or set the Parameter q2.
















   ..
       !! processed by numpydoc !!

.. py:property:: fc
   :type: Optional[float]


   
   Get or set the Critical void volume fraction fc.
















   ..
       !! processed by numpydoc !!

.. py:property:: f0
   :type: Optional[float]


   
   Get or set the Initial void volume fraction f0.
















   ..
       !! processed by numpydoc !!

.. py:property:: en
   :type: Optional[float]


   
   Get or set the Mean nucleation strain En .
   GT.0.0: Constant value,
   LT.0.0: Load curve ID = (-EN) which defines mean nucleation strain ε_N  as a function of element length.
















   ..
       !! processed by numpydoc !!

.. py:property:: sn
   :type: Optional[float]


   
   Get or set the Standard deviation Sn of the normal distribution of En.
   GT.0.0: Constant value,
   LT.0.0: Load curve ID = (-SN) which defines standard deviation s_N of the normal distribution of ε_N as a function of element length.
















   ..
       !! processed by numpydoc !!

.. py:property:: fn
   :type: Optional[float]


   
   Get or set the Void volume fraction of nucleating particles.
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Hardening modulus. This value is only used if ATYP=2 and LCSS=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: atyp
   :type: float


   
   Get or set the Type of hardening.
   EQ.1.0 Power law.
   EQ.2.0: Linear hardening.
   EQ.3.0: 8 points curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: ff0
   :type: Optional[float]


   
   Get or set the Failure void volume fraction. This value is used if no curve is given by the points L1,FF1 - L4,FF4 and LCLF=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: Optional[float]


   
   Get or set the Effective plastic strain values.The first point must be zero corresponding to the initial yield stress. This option is only used if ATYP equal to 3. At least 2 points should be defined.These values are used if ATYP=3 and LCSS=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2
   :type: Optional[float]


   
   Get or set the Effective plastic strain values at point 2
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3
   :type: Optional[float]


   
   Get or set the Effective plastic strain values at point 3
















   ..
       !! processed by numpydoc !!

.. py:property:: eps4
   :type: Optional[float]


   
   Get or set the Effective plastic strain values at point 4
















   ..
       !! processed by numpydoc !!

.. py:property:: eps5
   :type: Optional[float]


   
   Get or set the Effective plastic strain values at point 5
















   ..
       !! processed by numpydoc !!

.. py:property:: eps6
   :type: Optional[float]


   
   Get or set the Effective plastic strain values at point 6
















   ..
       !! processed by numpydoc !!

.. py:property:: eps7
   :type: Optional[float]


   
   Get or set the Effective plastic strain values at point 7
















   ..
       !! processed by numpydoc !!

.. py:property:: eps8
   :type: Optional[float]


   
   Get or set the Effective plastic strain values at point 8
















   ..
       !! processed by numpydoc !!

.. py:property:: es1
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS1 - EPS8. These values are used if ATYP=3 and LCSS=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: es2
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS2
















   ..
       !! processed by numpydoc !!

.. py:property:: es3
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS3
















   ..
       !! processed by numpydoc !!

.. py:property:: es4
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS4
















   ..
       !! processed by numpydoc !!

.. py:property:: es5
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS5
















   ..
       !! processed by numpydoc !!

.. py:property:: es6
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS6
















   ..
       !! processed by numpydoc !!

.. py:property:: es7
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS7
















   ..
       !! processed by numpydoc !!

.. py:property:: es8
   :type: Optional[float]


   
   Get or set the Corresponding yield stress values to EPS8
















   ..
       !! processed by numpydoc !!

.. py:property:: l1
   :type: Optional[float]


   
   Get or set the Element length values.These values are only used if LCLF=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: l2
   :type: Optional[float]


   
   Get or set the Element length values.
















   ..
       !! processed by numpydoc !!

.. py:property:: l3
   :type: Optional[float]


   
   Get or set the Element length values.
















   ..
       !! processed by numpydoc !!

.. py:property:: l4
   :type: Optional[float]


   
   Get or set the Element length values.
















   ..
       !! processed by numpydoc !!

.. py:property:: ff1
   :type: Optional[float]


   
   Get or set the Corresponding failure void volume fraction. These values are only used if LCLF=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: ff2
   :type: Optional[float]


   
   Get or set the Corresponding failure void volume fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ff3
   :type: Optional[float]


   
   Get or set the Corresponding failure void volume fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ff4
   :type: Optional[float]


   
   Get or set the Corresponding failure void volume fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: int


   
   Get or set the Load curve ID defining effective stress versus effective plastic strain. ATYP is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: lclf
   :type: int


   
   Get or set the Load curve ID defining failure void volume fraction versus element length. The values L1-L4 and FF1-FF4 are ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: numint
   :type: float


   
   Get or set the Number of through thickness integration points which must fail before the element is deleted.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Parameter alpha. for the Rc-Dc model
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Parameter beta. for the Rc-Dc model
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Parameter gamma. for the Rc-Dc model
















   ..
       !! processed by numpydoc !!

.. py:property:: d0
   :type: Optional[float]


   
   Get or set the Parameter D0. for the Rc-Dc model
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Parameter b. for the Rc-Dc model
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_
   :type: Optional[float]


   
   Get or set the Parameter lambda. for the Rc-Dc model
















   ..
       !! processed by numpydoc !!

.. py:property:: ds
   :type: Optional[float]


   
   Get or set the Parameter Ds. for the Rc-Dc model
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the Characteristic element length for this material
















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
   :value: 'GURSON_RCDC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





