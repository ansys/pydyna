





:class:`MatWood`
================


.. py:class:: mat_wood.MatWood(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_WOOD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatWood

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
          * - :py:attr:`~el`
            - Get or set the Parallel normal modulus.
          * - :py:attr:`~et`
            - Get or set the Perpendicular normal modulus.
          * - :py:attr:`~glt`
            - Get or set the Parallel shear modulus (GLT=GLR).
          * - :py:attr:`~gtr`
            - Get or set the Perpendicular shear modulus.
          * - :py:attr:`~pr`
            - Get or set the Parallel major Poisson's ratio.
          * - :py:attr:`~xt`
            - Get or set the Parallel tensile strength.
          * - :py:attr:`~xc`
            - Get or set the Parallel compressive strength.
          * - :py:attr:`~yt`
            - Get or set the Perpendicular tensile strength.
          * - :py:attr:`~yc`
            - Get or set the Perpendicular compressive strength.
          * - :py:attr:`~sxy`
            - Get or set the Parallel shear strength.
          * - :py:attr:`~syz`
            - Get or set the Perpendicular shear strength.
          * - :py:attr:`~gf1__`
            - Get or set the Parallel fracture energy in tension.
          * - :py:attr:`~gf2__`
            - Get or set the Parallel fracture energy in shear.
          * - :py:attr:`~bfit`
            - Get or set the Parallel softening parameter.
          * - :py:attr:`~dmax__`
            - Get or set the Parallel maximum damage.
          * - :py:attr:`~gf1p`
            - Get or set the Perpendicular fracture energy in tension.
          * - :py:attr:`~gf2p`
            - Get or set the Perpendicular fracture energy in shear.
          * - :py:attr:`~dfit`
            - Get or set the Perpendicular softening parameter.
          * - :py:attr:`~dmaxp`
            - Get or set the Perpendicular maxiumum damage.
          * - :py:attr:`~flpar`
            - Get or set the Parallel fluidity parameter for tesion and shear.
          * - :py:attr:`~flparc`
            - Get or set the Parallel fluidity parameter for compresion.
          * - :py:attr:`~powpar`
            - Get or set the Parallel power.
          * - :py:attr:`~flper`
            - Get or set the Perpendicular fluidity parameter for tension and shear.
          * - :py:attr:`~flperc`
            - Get or set the Perpendicular fluidity parameter for compression.
          * - :py:attr:`~powper`
            - Get or set the Perpendicular power.
          * - :py:attr:`~npar`
            - Get or set the Parallel hardening initiation.
          * - :py:attr:`~cpar`
            - Get or set the Parallel hardening rate.
          * - :py:attr:`~nper`
            - Get or set the Perpendicular hardening initiation.
          * - :py:attr:`~cper`
            - Get or set the Perpendicular hardening rate.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT=3.
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

    from mat_wood import MatWood

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

.. py:property:: el
   :type: Optional[float]


   
   Get or set the Parallel normal modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: et
   :type: Optional[float]


   
   Get or set the Perpendicular normal modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: glt
   :type: Optional[float]


   
   Get or set the Parallel shear modulus (GLT=GLR).
















   ..
       !! processed by numpydoc !!

.. py:property:: gtr
   :type: Optional[float]


   
   Get or set the Perpendicular shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Parallel major Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Parallel tensile strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the Parallel compressive strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: Optional[float]


   
   Get or set the Perpendicular tensile strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the Perpendicular compressive strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: sxy
   :type: Optional[float]


   
   Get or set the Parallel shear strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: syz
   :type: Optional[float]


   
   Get or set the Perpendicular shear strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: gf1__
   :type: Optional[float]


   
   Get or set the Parallel fracture energy in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: gf2__
   :type: Optional[float]


   
   Get or set the Parallel fracture energy in shear.
















   ..
       !! processed by numpydoc !!

.. py:property:: bfit
   :type: Optional[float]


   
   Get or set the Parallel softening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmax__
   :type: Optional[float]


   
   Get or set the Parallel maximum damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: gf1p
   :type: Optional[float]


   
   Get or set the Perpendicular fracture energy in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: gf2p
   :type: Optional[float]


   
   Get or set the Perpendicular fracture energy in shear.
















   ..
       !! processed by numpydoc !!

.. py:property:: dfit
   :type: Optional[float]


   
   Get or set the Perpendicular softening parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmaxp
   :type: Optional[float]


   
   Get or set the Perpendicular maxiumum damage.
















   ..
       !! processed by numpydoc !!

.. py:property:: flpar
   :type: Optional[float]


   
   Get or set the Parallel fluidity parameter for tesion and shear.
















   ..
       !! processed by numpydoc !!

.. py:property:: flparc
   :type: Optional[float]


   
   Get or set the Parallel fluidity parameter for compresion.
















   ..
       !! processed by numpydoc !!

.. py:property:: powpar
   :type: Optional[float]


   
   Get or set the Parallel power.
















   ..
       !! processed by numpydoc !!

.. py:property:: flper
   :type: Optional[float]


   
   Get or set the Perpendicular fluidity parameter for tension and shear.
















   ..
       !! processed by numpydoc !!

.. py:property:: flperc
   :type: Optional[float]


   
   Get or set the Perpendicular fluidity parameter for compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: powper
   :type: Optional[float]


   
   Get or set the Perpendicular power.
















   ..
       !! processed by numpydoc !!

.. py:property:: npar
   :type: Optional[float]


   
   Get or set the Parallel hardening initiation.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpar
   :type: Optional[float]


   
   Get or set the Parallel hardening rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: nper
   :type: Optional[float]


   
   Get or set the Perpendicular hardening initiation.
















   ..
       !! processed by numpydoc !!

.. py:property:: cper
   :type: Optional[float]


   
   Get or set the Perpendicular hardening rate.
















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

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for solid elements:
   EQ.1 : No change, default
   EQ.2 : Switch material axes a and b after BETA rotation
   EQ.3 : Switch material axes a and c after BETA rotation
   EQ.4 : Switch material axes b and c after BETA rotation
   EQ. - 4 : Switch material axes b and c before BETA rotation
   EQ. - 3 : Switch material axes a and c before BETA rotation
   EQ. - 2 : Switch material axes a and b before BETA rotation
   Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT=3.
















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
   :value: 'WOOD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





