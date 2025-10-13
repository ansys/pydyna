





:class:`MatOrthoElasticPlastic`
===============================


.. py:class:: mat_ortho_elastic_plastic.MatOrthoElasticPlastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ORTHO_ELASTIC_PLASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatOrthoElasticPlastic

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
          * - :py:attr:`~e11`
            - Get or set the Young's modulus in 11-direction.
          * - :py:attr:`~e22`
            - Get or set the Young's modulus in 22-direction.
          * - :py:attr:`~g12`
            - Get or set the Shear modulus in 12-direction.
          * - :py:attr:`~pr12`
            - Get or set the Poisson's ratio 12.
          * - :py:attr:`~pr23`
            - Get or set the Poisson's ratio 23.
          * - :py:attr:`~pr31`
            - Get or set the Poisson's ratio 31.
          * - :py:attr:`~sigma0`
            - Get or set the Initial yield stress.
          * - :py:attr:`~lc`
            - Get or set the Load curve ID or Table ID. The load curve ID defines effective stress versus effective plastic strain.
          * - :py:attr:`~qr1`
            - Get or set the Isotropic hardening parameter QR1.
          * - :py:attr:`~cr1`
            - Get or set the Isotropic hardening parameter CR1.
          * - :py:attr:`~qr2`
            - Get or set the Isotropic hardening parameter QR2.
          * - :py:attr:`~cr2`
            - Get or set the Isotropic hardening parameter CR2.
          * - :py:attr:`~r11`
            - Get or set the Yield criteria parameter R11
          * - :py:attr:`~r22`
            - Get or set the Yield criteria parameter R22
          * - :py:attr:`~r33`
            - Get or set the Yield criteria parameter R33
          * - :py:attr:`~r12`
            - Get or set the Yield criteria parameter R12
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BEAT or *ELEMENT_SOLID_ORTHO.
          * - :py:attr:`~xp`
            - Get or set the x-coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the y-coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the z-coordinates of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the component of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the component of vector d for AOPT = 2.
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

    from mat_ortho_elastic_plastic import MatOrthoElasticPlastic

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

.. py:property:: e11
   :type: Optional[float]


   
   Get or set the Young's modulus in 11-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: e22
   :type: Optional[float]


   
   Get or set the Young's modulus in 22-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: g12
   :type: Optional[float]


   
   Get or set the Shear modulus in 12-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr12
   :type: Optional[float]


   
   Get or set the Poisson's ratio 12.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr23
   :type: Optional[float]


   
   Get or set the Poisson's ratio 23.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr31
   :type: Optional[float]


   
   Get or set the Poisson's ratio 31.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma0
   :type: Optional[float]


   
   Get or set the Initial yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc
   :type: Optional[int]


   
   Get or set the Load curve ID or Table ID. The load curve ID defines effective stress versus effective plastic strain.
















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

.. py:property:: r11
   :type: Optional[float]


   
   Get or set the Yield criteria parameter R11
















   ..
       !! processed by numpydoc !!

.. py:property:: r22
   :type: Optional[float]


   
   Get or set the Yield criteria parameter R22
















   ..
       !! processed by numpydoc !!

.. py:property:: r33
   :type: Optional[float]


   
   Get or set the Yield criteria parameter R33
















   ..
       !! processed by numpydoc !!

.. py:property:: r12
   :type: Optional[float]


   
   Get or set the Yield criteria parameter R12
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the normal to the plane of the element.
   LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BEAT or *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinates of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the component of vector d for AOPT = 2.
















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
   :value: 'ORTHO_ELASTIC_PLASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





