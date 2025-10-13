





:class:`MatCompositeMatrix`
===========================


.. py:class:: mat_composite_matrix.MatCompositeMatrix(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_COMPOSITE_MATRIX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCompositeMatrix

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
          * - :py:attr:`~c11`
            - Get or set the C11 coefficient of the stiffness matrix.
          * - :py:attr:`~c12`
            - Get or set the C12 coefficient of the stiffness matrix.
          * - :py:attr:`~c22`
            - Get or set the C22 coefficient of the stiffness matrix.
          * - :py:attr:`~c13`
            - Get or set the C13 coefficient of the stiffness matrix.
          * - :py:attr:`~c23`
            - Get or set the C23 coefficient of the stiffness matrix.
          * - :py:attr:`~c33`
            - Get or set the C33 coefficient of the stiffness matrix.
          * - :py:attr:`~c14`
            - Get or set the C14 coefficient of the stiffness matrix.
          * - :py:attr:`~c24`
            - Get or set the C24 coefficient of the stiffness matrix.
          * - :py:attr:`~c34`
            - Get or set the C34 coefficient of the stiffness matrix.
          * - :py:attr:`~c44`
            - Get or set the C44 coefficient of the stiffness matrix.
          * - :py:attr:`~c15`
            - Get or set the C15 coefficient of the stiffness matrix.
          * - :py:attr:`~c25`
            - Get or set the C25 coefficient of the stiffness matrix.
          * - :py:attr:`~c35`
            - Get or set the C35 coefficient of the stiffness matrix.
          * - :py:attr:`~c45`
            - Get or set the C45 coefficient of the stiffness matrix.
          * - :py:attr:`~c55`
            - Get or set the C55 coefficient of the stiffness matrix.
          * - :py:attr:`~c16`
            - Get or set the C16 coefficient of the stiffness matrix.
          * - :py:attr:`~c26`
            - Get or set the C26 coefficient of the stiffness matrix.
          * - :py:attr:`~c36`
            - Get or set the C36 coefficient of the stiffness matrix.
          * - :py:attr:`~c46`
            - Get or set the C46 coefficient of the stiffness matrix.
          * - :py:attr:`~c56`
            - Get or set the C56 coefficient of the stiffness matrix.
          * - :py:attr:`~c66`
            - Get or set the C66 coefficient of the stiffness matrix.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~xp`
            - Get or set the Defines x-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the Defines y-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the Defines z-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Defines component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Defines component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Defines component of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Defines component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Defines component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Defines component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Defines component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Defines component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Defines component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
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

    from mat_composite_matrix import MatCompositeMatrix

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

.. py:property:: c11
   :type: Optional[float]


   
   Get or set the C11 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c12
   :type: Optional[float]


   
   Get or set the C12 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c22
   :type: Optional[float]


   
   Get or set the C22 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c13
   :type: Optional[float]


   
   Get or set the C13 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: Optional[float]


   
   Get or set the C23 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c33
   :type: Optional[float]


   
   Get or set the C33 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c14
   :type: Optional[float]


   
   Get or set the C14 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c24
   :type: Optional[float]


   
   Get or set the C24 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c34
   :type: Optional[float]


   
   Get or set the C34 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c44
   :type: Optional[float]


   
   Get or set the C44 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c15
   :type: Optional[float]


   
   Get or set the C15 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c25
   :type: Optional[float]


   
   Get or set the C25 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c35
   :type: Optional[float]


   
   Get or set the C35 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c45
   :type: Optional[float]


   
   Get or set the C45 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c55
   :type: Optional[float]


   
   Get or set the C55 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c16
   :type: Optional[float]


   
   Get or set the C16 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c26
   :type: Optional[float]


   
   Get or set the C26 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c36
   :type: Optional[float]


   
   Get or set the C36 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c46
   :type: Optional[float]


   
   Get or set the C46 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c56
   :type: Optional[float]


   
   Get or set the C56 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: c66
   :type: Optional[float]


   
   Get or set the C66 coefficient of the stiffness matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4 of an element are identical to the nodes used for the definition of a coordinate system as by *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Defines x-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Defines y-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Defines z-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Defines component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Defines component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Defines component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Defines component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Defines component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Defines component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Defines component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Defines component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Defines component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
















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
   :value: 'COMPOSITE_MATRIX'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





