





:class:`MatWoodPine`
====================


.. py:class:: mat_wood_pine.MatWoodPine(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_WOOD_PINE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatWoodPine

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~nplot`
            - Get or set the Plotting Options:
          * - :py:attr:`~iters`
            - Get or set the Number of plasticity algorithm iterations.  The default is one iteration.
          * - :py:attr:`~irate`
            - Get or set the Rate effects option:
          * - :py:attr:`~ghard`
            - Get or set the Perfect plasticity override. Values greater than or equal to zero are allowed. Positive values model late time hardening in compression (an increase in strength with increasing strain). A zero value models perfect plasticity (no increase in strength with increasing strain). The default is zero.
          * - :py:attr:`~ifail`
            - Get or set the Erosion perpendicular to the grain.
          * - :py:attr:`~ivol`
            - Get or set the Erode on negative volume or strain increments greater than 0.01.
          * - :py:attr:`~mois`
            - Get or set the Percent moisture content. If left blank, moisture content defaults to saturated at 30%.
          * - :py:attr:`~temp`
            - Get or set the Temperature in C. If left blank, temperature defaults to room temperature at 20 C.
          * - :py:attr:`~qual_t`
            - Get or set the Quality factor options. These quality factors reduce the clear wood tension, shear, and compression strengths as a function of grade.
          * - :py:attr:`~qual_c`
            - Get or set the User defined quality factor in compression. This input value is used if Qual_T>0. Values between 0 and 1 are expected. Values greater than one are allowed, but may not be realistic. If left blank, a default value of Qual_C=Qual_T is used.
          * - :py:attr:`~units`
            - Get or set the Units options:
          * - :py:attr:`~iqual`
            - Get or set the Apply quality factors perpendicular to the grain:
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
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
            - Get or set the Components of vector a for AOPT = 2.
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Define components of vector v for AOPT = 3 and 4.
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

    from mat_wood_pine import MatWoodPine

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplot
   :type: int


   
   Get or set the Plotting Options:
   EQ.1. Parallel damage (default).
   EQ.2. Perpendicular damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: iters
   :type: int


   
   Get or set the Number of plasticity algorithm iterations.  The default is one iteration.
   GE.0:   Original plasticity iteration developed by Murray [2002].
   LT.0:   Plasticity iteration (return mapping) with non-associated flow direction for perpendicular yielding. The absolute value of ITERS is used as number of plasticity algorithm iterations.
















   ..
       !! processed by numpydoc !!

.. py:property:: irate
   :type: int


   
   Get or set the Rate effects option:
   EQ.0. Rate effects model turned off (default).
   EQ.1. Rate effects model turned on.on with the original rate dependence described by Murray [2002].
   EQ.2:   Rate effects model turned on with Johnson-Cook like rate dependence of the strength parameters, as described below in the remarks. Only works in combination with ITERS.LT.0 and OPTION=<BLANK>..
















   ..
       !! processed by numpydoc !!

.. py:property:: ghard
   :type: float


   
   Get or set the Perfect plasticity override. Values greater than or equal to zero are allowed. Positive values model late time hardening in compression (an increase in strength with increasing strain). A zero value models perfect plasticity (no increase in strength with increasing strain). The default is zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifail
   :type: int


   
   Get or set the Erosion perpendicular to the grain.
   EQ.0. No (default).
   EQ.1. Yes (not recommended except for debugging).
















   ..
       !! processed by numpydoc !!

.. py:property:: ivol
   :type: int


   
   Get or set the Erode on negative volume or strain increments greater than 0.01.
   EQ.0:  No, do not apply erosion criteria.
   EQ.1:  Yes, apply volume and strain erosion criteria
















   ..
       !! processed by numpydoc !!

.. py:property:: mois
   :type: Optional[float]


   
   Get or set the Percent moisture content. If left blank, moisture content defaults to saturated at 30%.
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: Optional[float]


   
   Get or set the Temperature in C. If left blank, temperature defaults to room temperature at 20 C.
















   ..
       !! processed by numpydoc !!

.. py:property:: qual_t
   :type: Optional[float]


   
   Get or set the Quality factor options. These quality factors reduce the clear wood tension, shear, and compression strengths as a function of grade.
   EQ. 1: Grade 1, 1D, 2, 2D.
   Predefined strength reduction factors are:
   Pine: Qual_T=0.47 in tension/shear.
   Qual_C=0.63 in compression.
   Fir: Qual_T=0.040 in tension/shear.
   Qual_C=0.73 in compression.
   EQ.-1: DS-65 or SEl STR (pine and fir).
   Predefined strength reduction factors are:
   Qual_T=0.80 in tension/shear.
   Qual_C=0.93 in compression.
   EQ.-2: Clear wood.
   No strength reduction factors are applied:
   Qual_T=1.0.
   Qual_C=1.0.
   GT.0: User defined quality factor in tension. Values between 0 and 1 are expected. Values greater than one are allowed, but not be realistic.
















   ..
       !! processed by numpydoc !!

.. py:property:: qual_c
   :type: Optional[float]


   
   Get or set the User defined quality factor in compression. This input value is used if Qual_T>0. Values between 0 and 1 are expected. Values greater than one are allowed, but may not be realistic. If left blank, a default value of Qual_C=Qual_T is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: units
   :type: int


   
   Get or set the Units options:
   EQ.0: GPa, mm, msec, Kg/mm^3, kN.
   EQ.1: MPa, cm, msec, g/mm^3, Nt.
   EQ.2: MPa, mm, sec, Mg/mm^3, Nt.
   EQ.3: Psi, inch, sec, lb-s^2/inch^4, lb.
















   ..
       !! processed by numpydoc !!

.. py:property:: iqual
   :type: int


   
   Get or set the Apply quality factors perpendicular to the grain:
   EQ.0: Yes (default).
   EQ.1: No.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
   EQ.2.0: Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
   EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
















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


   
   Get or set the Components of vector a for AOPT = 2.
















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


   
   Get or set the Components of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4.
















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
   :value: 'WOOD_PINE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





