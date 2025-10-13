





:class:`MatJohnsonHolmquistJh1`
===============================


.. py:class:: mat_johnson_holmquist_jh1.MatJohnsonHolmquistJh1(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_JOHNSON_HOLMQUIST_JH1 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatJohnsonHolmquistJh1

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
            - Get or set the Density
          * - :py:attr:`~g`
            - Get or set the Shear modulus
          * - :py:attr:`~p1`
            - Get or set the Pressure point 1 for intact material
          * - :py:attr:`~s1`
            - Get or set the Effective stress at P1
          * - :py:attr:`~p2`
            - Get or set the Pressure point 2 for intact material
          * - :py:attr:`~s2`
            - Get or set the Effective stress at P2
          * - :py:attr:`~c`
            - Get or set the Strain rate sensitivity factor.
          * - :py:attr:`~epsi`
            - Get or set the Quasi-static threshold strain rate. See *MAT_015.
          * - :py:attr:`~t`
            - Get or set the Maximum tensile pressure strength. This value is positive in tension.
          * - :py:attr:`~alpha`
            - Get or set the Slope of the fractured material strength curve.
          * - :py:attr:`~sfmax`
            - Get or set the Maximum strength of the fractured material.
          * - :py:attr:`~beta`
            - Get or set the Fraction of elastic energy loss converted to hydrostatic energy (affects bulking pressure (history variable 1) that accompanies damage)..
          * - :py:attr:`~dp1`
            - Get or set the Maximum compressive pressure strength. This value is positive in compression.
          * - :py:attr:`~epfmin`
            - Get or set the Plastic strain for fracture at tensile pressure.
          * - :py:attr:`~epfmax`
            - Get or set the Plastic strain for fracture at DP1.
          * - :py:attr:`~k1`
            - Get or set the First pressure coefficient (equivalent to the bulk modulus).
          * - :py:attr:`~k2`
            - Get or set the Second pressure coefficient.
          * - :py:attr:`~k3`
            - Get or set the Third pressure coefficient.
          * - :py:attr:`~fs`
            - Get or set the Element deletion criteria.
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

    from mat_johnson_holmquist_jh1 import MatJohnsonHolmquistJh1

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Density
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the Pressure point 1 for intact material
















   ..
       !! processed by numpydoc !!

.. py:property:: s1
   :type: Optional[float]


   
   Get or set the Effective stress at P1
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Pressure point 2 for intact material
















   ..
       !! processed by numpydoc !!

.. py:property:: s2
   :type: Optional[float]


   
   Get or set the Effective stress at P2
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Strain rate sensitivity factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsi
   :type: Optional[float]


   
   Get or set the Quasi-static threshold strain rate. See *MAT_015.
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Maximum tensile pressure strength. This value is positive in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Slope of the fractured material strength curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfmax
   :type: Optional[float]


   
   Get or set the Maximum strength of the fractured material.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Fraction of elastic energy loss converted to hydrostatic energy (affects bulking pressure (history variable 1) that accompanies damage)..
















   ..
       !! processed by numpydoc !!

.. py:property:: dp1
   :type: Optional[float]


   
   Get or set the Maximum compressive pressure strength. This value is positive in compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: epfmin
   :type: Optional[float]


   
   Get or set the Plastic strain for fracture at tensile pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: epfmax
   :type: Optional[float]


   
   Get or set the Plastic strain for fracture at DP1.
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the First pressure coefficient (equivalent to the bulk modulus).
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the Second pressure coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: k3
   :type: Optional[float]


   
   Get or set the Third pressure coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: Optional[float]


   
   Get or set the Element deletion criteria.
   FS < 0 delete if P < FS (tensile failure).
   FS = 0 no element deletion (default)..
   FS> 0 delete element if the strain > FS.
















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
   :value: 'JOHNSON_HOLMQUIST_JH1'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





