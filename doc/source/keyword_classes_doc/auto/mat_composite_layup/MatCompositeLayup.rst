





:class:`MatCompositeLayup`
==========================


.. py:class:: mat_composite_layup.MatCompositeLayup(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_COMPOSITE_LAYUP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCompositeLayup

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
          * - :py:attr:`~ea`
            - Get or set the Young's modulus in a-direction.
          * - :py:attr:`~eb`
            - Get or set the Young's modulus in b-direction.
          * - :py:attr:`~ec`
            - Get or set the Young's modulus in c-direction.
          * - :py:attr:`~prba`
            - Get or set the Poisson's ratio ba.
          * - :py:attr:`~prca`
            - Get or set the Poisson's ratio ca.
          * - :py:attr:`~prcb`
            - Get or set the Poisson's ratio cb.
          * - :py:attr:`~gab`
            - Get or set the Shear modulus ab.
          * - :py:attr:`~gbc`
            - Get or set the Shear modulus bc.
          * - :py:attr:`~gca`
            - Get or set the Shear modulus ca.
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

    from mat_composite_layup import MatCompositeLayup

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

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Young's modulus in a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Young's modulus in b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Young's modulus in c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Poisson's ratio ba.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the Poisson's ratio ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Poisson's ratio cb.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Shear modulus ab.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the Shear modulus bc.
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the Shear modulus ca.
















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
   :value: 'COMPOSITE_LAYUP'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





