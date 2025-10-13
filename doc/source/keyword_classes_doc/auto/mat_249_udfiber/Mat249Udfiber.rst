





:class:`Mat249Udfiber`
======================


.. py:class:: mat_249_udfiber.Mat249Udfiber(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_249_UDfiber keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat249Udfiber

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
            - Get or set the Density
          * - :py:attr:`~em`
            - Get or set the Isotropic young's modulus
          * - :py:attr:`~prm`
            - Get or set the Poisson's ratio for matrix material
          * - :py:attr:`~g`
            - Get or set the Linear shear modulus
          * - :py:attr:`~ezdef`
            - Get or set the Algorithmic parameter. If set to 1, last row of deformation gradient is not updated during the calculation
          * - :py:attr:`~nfib`
            - Get or set the Number of fiber families to be considered
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
          * - :py:attr:`~xp`
            - Get or set the X-coordinate of point p for AOPT = 1
          * - :py:attr:`~yp`
            - Get or set the Y-coordinate of point p for AOPT = 1
          * - :py:attr:`~zp`
            - Get or set the Z-coordinate of point p for AOPT = 1
          * - :py:attr:`~a1`
            - Get or set the X-component of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Y-component of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Z-component of vector a for AOPT = 2
          * - :py:attr:`~v1`
            - Get or set the X-component of vector v for AOPT = 3
          * - :py:attr:`~v2`
            - Get or set the Y-component of vector v for AOPT = 3
          * - :py:attr:`~v3`
            - Get or set the Z-component of vector v for AOPT = 3
          * - :py:attr:`~d1`
            - Get or set the X-component of vector d for AOPT = 2
          * - :py:attr:`~d2`
            - Get or set the Y-component of vector d for AOPT = 2
          * - :py:attr:`~d3`
            - Get or set the Z-component of vector d for AOPT = 2
          * - :py:attr:`~mangl`
            - Get or set the Material angle in degrees for AOPT = 0 and 3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA
          * - :py:attr:`~idf1`
            - Get or set the ID for i-th fiber family for post-processing
          * - :py:attr:`~alph1`
            - Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
          * - :py:attr:`~ef1`
            - Get or set the Young's modulus for i-th fiber family
          * - :py:attr:`~kap1`
            - Get or set the Fiber volume ratio for i-th fiber family
          * - :py:attr:`~idf2`
            - Get or set the ID for i-th fiber family for post-processing
          * - :py:attr:`~alph2`
            - Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
          * - :py:attr:`~ef2`
            - Get or set the Young's modulus for i-th fiber family
          * - :py:attr:`~kap2`
            - Get or set the Fiber volume ratio for i-th fiber family
          * - :py:attr:`~idf3`
            - Get or set the ID for i-th fiber family for post-processing
          * - :py:attr:`~alph3`
            - Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
          * - :py:attr:`~ef3`
            - Get or set the Young's modulus for i-th fiber family
          * - :py:attr:`~kap3`
            - Get or set the Fiber volume ratio for i-th fiber family
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

    from mat_249_udfiber import Mat249Udfiber

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Density
















   ..
       !! processed by numpydoc !!

.. py:property:: em
   :type: Optional[float]


   
   Get or set the Isotropic young's modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: prm
   :type: Optional[float]


   
   Get or set the Poisson's ratio for matrix material
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Linear shear modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: ezdef
   :type: Optional[float]


   
   Get or set the Algorithmic parameter. If set to 1, last row of deformation gradient is not updated during the calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: nfib
   :type: Optional[int]


   
   Get or set the Number of fiber families to be considered
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle MANGL.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the X-coordinate of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Y-coordinate of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Z-coordinate of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the X-component of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Y-component of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Z-component of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the X-component of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Y-component of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Z-component of vector v for AOPT = 3
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the X-component of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Y-component of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Z-component of vector d for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: mangl
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 0 and 3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA
















   ..
       !! processed by numpydoc !!

.. py:property:: idf1
   :type: Optional[int]


   
   Get or set the ID for i-th fiber family for post-processing
















   ..
       !! processed by numpydoc !!

.. py:property:: alph1
   :type: Optional[float]


   
   Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ef1
   :type: Optional[float]


   
   Get or set the Young's modulus for i-th fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: kap1
   :type: Optional[float]


   
   Get or set the Fiber volume ratio for i-th fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: idf2
   :type: Optional[int]


   
   Get or set the ID for i-th fiber family for post-processing
















   ..
       !! processed by numpydoc !!

.. py:property:: alph2
   :type: Optional[float]


   
   Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ef2
   :type: Optional[float]


   
   Get or set the Young's modulus for i-th fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: kap2
   :type: Optional[float]


   
   Get or set the Fiber volume ratio for i-th fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: idf3
   :type: Optional[int]


   
   Get or set the ID for i-th fiber family for post-processing
















   ..
       !! processed by numpydoc !!

.. py:property:: alph3
   :type: Optional[float]


   
   Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ef3
   :type: Optional[float]


   
   Get or set the Young's modulus for i-th fiber family
















   ..
       !! processed by numpydoc !!

.. py:property:: kap3
   :type: Optional[float]


   
   Get or set the Fiber volume ratio for i-th fiber family
















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
   :value: '249_UDfiber'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





