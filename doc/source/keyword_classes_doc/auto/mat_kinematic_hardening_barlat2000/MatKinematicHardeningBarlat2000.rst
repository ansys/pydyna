





:class:`MatKinematicHardeningBarlat2000`
========================================


.. py:class:: mat_kinematic_hardening_barlat2000.MatKinematicHardeningBarlat2000(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_KINEMATIC_HARDENING_BARLAT2000 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatKinematicHardeningBarlat2000

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's Modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~ea`
            - Get or set the parameter controlling the change of Young’s modulus;
          * - :py:attr:`~coe`
            - Get or set the parameter controlling the change of Young’s modulus; see the remarks of *MAT_125.
          * - :py:attr:`~m`
            - Get or set the m, Flow potential exponent.  For face centered cubic (FCC) materials m=8 is recommended and for body centered cubic (BCC) materials m=6 may be used.
          * - :py:attr:`~alpha1`
            - Get or set the a1, material constant in Barlat's yield equation.
          * - :py:attr:`~alpha2`
            - Get or set the a2, material constant in Barlat's yield equation.
          * - :py:attr:`~alpha3`
            - Get or set the a3, material constant in Barlat's yield equation.
          * - :py:attr:`~alpha4`
            - Get or set the a4, material constant in Barlat's yield equation.
          * - :py:attr:`~alpha5`
            - Get or set the a5, material constant in Barlat's yield equation.
          * - :py:attr:`~alpha6`
            - Get or set the a6, material constant in Barlat's yield equation.
          * - :py:attr:`~alpha7`
            - Get or set the a7, material constant in Barlat's yield equation.
          * - :py:attr:`~alpha8`
            - Get or set the a8, material constant in Barlat's yield equation.
          * - :py:attr:`~cb`
            - Get or set the The uppercase B defined in the Yoshida's equations.
          * - :py:attr:`~y`
            - Get or set the Hardening parameter as defined in the Yoshida's equations.
          * - :py:attr:`~sc1`
            - Get or set the The lowercase c defined in the Yoshida's equations.
          * - :py:attr:`~k`
            - Get or set the Hardening parameter as defined in the Yoshida's equations.
          * - :py:attr:`~rsat`
            - Get or set the Hardening parameter as defined in the Yoshida's equations.
          * - :py:attr:`~sb`
            - Get or set the The lowercase b as defined in the Yoshida's equations
          * - :py:attr:`~h`
            - Get or set the Anisotropic parameter associated with work-hardening stagnation, defined in the Yoshida's equations.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~iopt`
            - Get or set the Kinematic hardening rule flag:
          * - :py:attr:`~c1`
            - Get or set the Constants used to modify R:
          * - :py:attr:`~c2`
            - Get or set the Constants used to modify R:
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2
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

    from mat_kinematic_hardening_barlat2000 import MatKinematicHardeningBarlat2000

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's Modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the parameter controlling the change of Young’s modulus;
   LT.0.0: |EA| is a curve ID giving the change of Young’s modulus as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: coe
   :type: Optional[float]


   
   Get or set the parameter controlling the change of Young’s modulus; see the remarks of *MAT_125.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the m, Flow potential exponent.  For face centered cubic (FCC) materials m=8 is recommended and for body centered cubic (BCC) materials m=6 may be used.
   LT.0.0: |M | is a load curve ID specifying the flow potential exponent as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the a1, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the a2, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the a3, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha4
   :type: Optional[float]


   
   Get or set the a4, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha5
   :type: Optional[float]


   
   Get or set the a5, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha6
   :type: Optional[float]


   
   Get or set the a6, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha7
   :type: Optional[float]


   
   Get or set the a7, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha8
   :type: Optional[float]


   
   Get or set the a8, material constant in Barlat's yield equation.
   LT.0.0: |ALPHAi| is a load curve ID specifying α_i as a function of effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: cb
   :type: Optional[float]


   
   Get or set the The uppercase B defined in the Yoshida's equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Hardening parameter as defined in the Yoshida's equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc1
   :type: Optional[float]


   
   Get or set the The lowercase c defined in the Yoshida's equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Hardening parameter as defined in the Yoshida's equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: rsat
   :type: Optional[float]


   
   Get or set the Hardening parameter as defined in the Yoshida's equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: sb
   :type: Optional[float]


   
   Get or set the The lowercase b as defined in the Yoshida's equations
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Anisotropic parameter associated with work-hardening stagnation, defined in the Yoshida's equations.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[int]


   
   Get or set the Material axes option:
   EQ.0: locally orthotropic with material axes determined by
   element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES.
   EQ.2: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
   EQ.3: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   LT.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
   *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
















   ..
       !! processed by numpydoc !!

.. py:property:: iopt
   :type: int


   
   Get or set the Kinematic hardening rule flag:
   EQ.0: Original Yoshida formulation,
   EQ.1: Modified formulation. Define C1, C2 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Constants used to modify R:
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Constants used to modify R:
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Components of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2
















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
   :value: 'KINEMATIC_HARDENING_BARLAT2000'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





