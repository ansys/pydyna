





:class:`MatExtended3ParameterBarlat`
====================================


.. py:class:: mat_extended_3_parameter_barlat.MatExtended3ParameterBarlat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_EXTENDED_3-PARAMETER_BARLAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatExtended3ParameterBarlat

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
          * - :py:attr:`~lch00`
            - Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
          * - :py:attr:`~lch45`
            - Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
          * - :py:attr:`~lch90`
            - Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
          * - :py:attr:`~lchbi`
            - Get or set the Load curve defining biaxial stress vs. biaxial strain, see discussion in the
          * - :py:attr:`~lchsh`
            - Get or set the Load curve defining shear stress vs. shear strain, see discussion in the
          * - :py:attr:`~lcr00`
            - Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
          * - :py:attr:`~lcr45`
            - Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
          * - :py:attr:`~lcr90`
            - Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
          * - :py:attr:`~lcrbi`
            - Get or set the Load curve defining biaxial R-value vs. biaxial strain, see discussion in the
          * - :py:attr:`~lcrsh`
            - Get or set the Load curve defining shear R-value vs. shear strain, see discussion in the
          * - :py:attr:`~m`
            - Get or set the Barlat flow exponent, , must be an integer value.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
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

    from mat_extended_3_parameter_barlat import MatExtended3ParameterBarlat

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

.. py:property:: lch00
   :type: Optional[int]


   
   Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
   (XX is either 00, 45, 90). The exact definition is discussed in the Remarks
   below. LCH00 must be defined, the other defaults to LCH00 if not defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lch45
   :type: Optional[int]


   
   Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
   (XX is either 00, 45, 90). The exact definition is discussed in the Remarks
   below. LCH00 must be defined, the other defaults to LCH00 if not defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lch90
   :type: Optional[int]


   
   Get or set the Load curve defining uniaxial stress vs. uniaxial strain in the given direction
   (XX is either 00, 45, 90). The exact definition is discussed in the Remarks
   below. LCH00 must be defined, the other defaults to LCH00 if not defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lchbi
   :type: Optional[int]


   
   Get or set the Load curve defining biaxial stress vs. biaxial strain, see discussion in the
   Remarks below for a definition. If not defined this is determined from
   LCH00 and the initial R-values to yield a response close to the standard 3-parameter Barlat model.
















   ..
       !! processed by numpydoc !!

.. py:property:: lchsh
   :type: Optional[int]


   
   Get or set the Load curve defining shear stress vs. shear strain, see discussion in the
   Remarks below for a definition. If not defined this is ignored to yield a
   response close to the standard 3-parameter Barlat model.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcr00
   :type: Optional[int]


   
   Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
   direction (XX is either 00, 45, 90). The exact definition is discussed in the
   Remarks below. Default is a constant R-value of 1.0, a negative input will
   result in a constant R-value of –LCRXX.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcr45
   :type: Optional[int]


   
   Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
   direction (XX is either 00, 45, 90). The exact definition is discussed in the
   Remarks below. Default is a constant R-value of 1.0, a negative input will
   result in a constant R-value of –LCRXX.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcr90
   :type: Optional[int]


   
   Get or set the Load curve defining standard R-value vs. uniaxial strain in the given
   direction (XX is either 00, 45, 90). The exact definition is discussed in the
   Remarks below. Default is a constant R-value of 1.0, a negative input will
   result in a constant R-value of –LCRXX.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcrbi
   :type: Optional[int]


   
   Get or set the Load curve defining biaxial R-value vs. biaxial strain, see discussion in the
   Remarks below for a definition. Default is a constant R-value of 1.0, a
   negative input will result in a constant R-value of –LCRBI.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcrsh
   :type: Optional[int]


   
   Get or set the Load curve defining shear R-value vs. shear strain, see discussion in the
   Remarks below for a definition. Default is a constant R-value of 1.0, a
   negative input will result in a constant R-value of –LCRSH.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Barlat flow exponent, , must be an integer value.
















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

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
















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
   :value: 'EXTENDED_3-PARAMETER_BARLAT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





