





:class:`MatAnisotropicPlastic`
==============================


.. py:class:: mat_anisotropic_plastic.MatAnisotropicPlastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ANISOTROPIC_PLASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAnisotropicPlastic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be choisen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~sigy`
            - Get or set the Initial yield stress.
          * - :py:attr:`~lcss`
            - Get or set the Load curve ID. The load curve ID defines effective stress versus effective plastic strain. Card 2 is ignored with this option.
          * - :py:attr:`~qr1`
            - Get or set the Isotropic hardening parameter QR1.
          * - :py:attr:`~cr1`
            - Get or set the Isotropic hardening parameter CR1.
          * - :py:attr:`~qr2`
            - Get or set the Isotropic hardening parameter QR2.
          * - :py:attr:`~cr2`
            - Get or set the Isotropic hardening parameter CR2.
          * - :py:attr:`~r00`
            - Get or set the R00 for anisotropic hardening.
          * - :py:attr:`~r45`
            - Get or set the R45 for anisotropic hardening.
          * - :py:attr:`~r90`
            - Get or set the R90 for anisotropic hardening.
          * - :py:attr:`~s11`
            - Get or set the Yield stress in local x-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~s22`
            - Get or set the Yield stress in local y-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~s33`
            - Get or set the Yield stress in local z-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~s12`
            - Get or set the Yield stress in local xy-direction. This input is ignored if (R00,R45,R90)>0.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~xp`
            - Get or set the Xp, define coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Yp, define coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Zp, define coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the A1, define coordinate of point a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the A2, define coordinate of point a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the A3, define coordinate of point a for AOPT = 2.
          * - :py:attr:`~d1`
            - Get or set the D1, define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d2`
            - Get or set the D2, define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d3`
            - Get or set the D3, define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v1`
            - Get or set the V1, define components of vector d for AOPT = 2.
          * - :py:attr:`~v2`
            - Get or set the V2, define components of vector d for AOPT = 2.
          * - :py:attr:`~v3`
            - Get or set the V3, define components of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
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

    from mat_anisotropic_plastic import MatAnisotropicPlastic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be choisen.
















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


   
   Get or set the Initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Load curve ID. The load curve ID defines effective stress versus effective plastic strain. Card 2 is ignored with this option.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter QR1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr1
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter CR1.
















   ..
       !! processed by numpydoc !!

.. py:property:: qr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter QR2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr2
   :type: Optional[float]


   
   Get or set the Isotropic hardening parameter CR2.
















   ..
       !! processed by numpydoc !!

.. py:property:: r00
   :type: Optional[float]


   
   Get or set the R00 for anisotropic hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: Optional[float]


   
   Get or set the R45 for anisotropic hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: Optional[float]


   
   Get or set the R90 for anisotropic hardening.
















   ..
       !! processed by numpydoc !!

.. py:property:: s11
   :type: Optional[float]


   
   Get or set the Yield stress in local x-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: s22
   :type: Optional[float]


   
   Get or set the Yield stress in local y-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: s33
   :type: Optional[float]


   
   Get or set the Yield stress in local z-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: s12
   :type: Optional[float]


   
   Get or set the Yield stress in local xy-direction. This input is ignored if (R00,R45,R90)>0.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Xp, define coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Yp, define coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Zp, define coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the A1, define coordinate of point a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the A2, define coordinate of point a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the A3, define coordinate of point a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the D1, define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the D2, define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the D3, define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the V1, define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the V2, define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the V3, define components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
















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
   :value: 'ANISOTROPIC_PLASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





