





:class:`MatKinematicHardeningBarlat89Nlp`
=========================================


.. py:class:: mat_kinematic_hardening_barlat89_nlp.MatKinematicHardeningBarlat89Nlp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_KINEMATIC_HARDENING_BARLAT89_NLP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatKinematicHardeningBarlat89Nlp

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
          * - :py:attr:`~m`
            - Get or set the m, exponent in Barlat's yield criterion.
          * - :py:attr:`~r00`
            - Get or set the R00, Lankford parameter in 0 degree direction.
          * - :py:attr:`~r45`
            - Get or set the R45, Lankford parameter in 45 degree direction.
          * - :py:attr:`~r90`
            - Get or set the R90, Lankford parameter in 90 degree direction.
          * - :py:attr:`~cb`
            - Get or set the The uppercase B defined in the Yoshida's equations.
          * - :py:attr:`~y`
            - Get or set the Hardening parameter as defined in the Yoshida's equations.
          * - :py:attr:`~sc`
            - Get or set the The lowercase c defined in the Yoshida's equations.
          * - :py:attr:`~k`
            - Get or set the Hardening parameter as defined in the Yoshida's equations.
          * - :py:attr:`~rsat`
            - Get or set the Hardening parameter as defined in the Yoshida's equations.
          * - :py:attr:`~sb`
            - Get or set the The lowercase b as defined in the Yoshida's equations
          * - :py:attr:`~h`
            - Get or set the Anisotropic parameter associated with work-hardening stagnation, defined in the Yoshida's equations.
          * - :py:attr:`~hlcid`
            - Get or set the Load curve ID in keyword *DEFINE_CURVE, where true strain and true       stress relationship is characterized.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~iopt`
            - Get or set the Kinematic hardening rule flag:
          * - :py:attr:`~c1`
            - Get or set the Constants used to modify R:
          * - :py:attr:`~c2`
            - Get or set the Constants used to modify R:
          * - :py:attr:`~ifld`
            - Get or set the ID of a load curve of the traditional Forming Limit Diagram (FLD) for the linear strain paths.  In the load curve, abscissas represent minor strains while ordinates represent major strains.  Define only when the NLP option is used.
          * - :py:attr:`~ea`
            - Get or set the Variable controlling the change of Young’s modulus, E^A
          * - :py:attr:`~coe`
            - Get or set the Variable controlling the change of Young’s modulus
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1.
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
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA
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

    from mat_kinematic_hardening_barlat89_nlp import MatKinematicHardeningBarlat89Nlp

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

.. py:property:: m
   :type: Optional[float]


   
   Get or set the m, exponent in Barlat's yield criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: r00
   :type: Optional[float]


   
   Get or set the R00, Lankford parameter in 0 degree direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: Optional[float]


   
   Get or set the R45, Lankford parameter in 45 degree direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: Optional[float]


   
   Get or set the R90, Lankford parameter in 90 degree direction.
















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

.. py:property:: sc
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

.. py:property:: hlcid
   :type: Optional[int]


   
   Get or set the Load curve ID in keyword *DEFINE_CURVE, where true strain and true       stress relationship is characterized.
















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

.. py:property:: ifld
   :type: Optional[int]


   
   Get or set the ID of a load curve of the traditional Forming Limit Diagram (FLD) for the linear strain paths.  In the load curve, abscissas represent minor strains while ordinates represent major strains.  Define only when the NLP option is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Variable controlling the change of Young’s modulus, E^A
















   ..
       !! processed by numpydoc !!

.. py:property:: coe
   :type: Optional[float]


   
   Get or set the Variable controlling the change of Young’s modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















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

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA
















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
   :value: 'KINEMATIC_HARDENING_BARLAT89_NLP'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





