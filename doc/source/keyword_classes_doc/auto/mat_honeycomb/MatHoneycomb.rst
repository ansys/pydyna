





:class:`MatHoneycomb`
=====================


.. py:class:: mat_honeycomb.MatHoneycomb(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_HONEYCOMB keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatHoneycomb

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
            - Get or set the Young's modulus for compacted honeycomb material.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio for compacted honeycomb material.
          * - :py:attr:`~sigy`
            - Get or set the Yield stress for fully compacted honeycomb.
          * - :py:attr:`~vf`
            - Get or set the Relative volume at which the honeycomb is fully compacted.
          * - :py:attr:`~mu`
            - Get or set the Material viscosity coefficient (default=.05) Recommended.
          * - :py:attr:`~bulk`
            - Get or set the Bulk viscosity flag:
          * - :py:attr:`~lca`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-aa versus either relative volume or volumetric strain.
          * - :py:attr:`~lcb`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-bb versus either relative volume or volumetric strain (default LCB=LCA).
          * - :py:attr:`~lcc`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-bb versus either relative volume or volumetric strain (default LCC=LCA).
          * - :py:attr:`~lcs`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for shear stress versus either relative volume or volumetric strain (default LCS=LCA).
          * - :py:attr:`~lcab`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-ab versus either relative volume or volumetric strain (default LCAB=LCS).
          * - :py:attr:`~lcbc`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-bc versus either relative volume or volumetric strain (default LCBC=LCS).
          * - :py:attr:`~lcca`
            - Get or set the Load curve ID, see *DEFINE_CURVE, or sigma-ca versus either relative volume or volumetric strain (default LCCA=LCS).
          * - :py:attr:`~lcsr`
            - Get or set the Load curve ID, see *DEFINE_CURVE, for strain-rate effects defining the scale factor versus strain rate (optional).
          * - :py:attr:`~eaau`
            - Get or set the Elastic modulus Eaau in uncompressed configuration.
          * - :py:attr:`~ebbu`
            - Get or set the Elastic modulus Ebbu in uncompressed configuration.
          * - :py:attr:`~eccu`
            - Get or set the Elastic modulus Eccu in uncompressed configuration.
          * - :py:attr:`~gabu`
            - Get or set the Shear modulus Gabu in uncompressed configuration.
          * - :py:attr:`~gbcu`
            - Get or set the Shear modulus Gbcu in uncompressed configuration.
          * - :py:attr:`~gcau`
            - Get or set the Shear modulus Gcau in uncompressed configuration.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the x-coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the y-coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the z-coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~tsef`
            - Get or set the Tensile strain at element failure (element will erode).
          * - :py:attr:`~ssef`
            - Get or set the Shear strain at element failure (element will erode).
          * - :py:attr:`~v1`
            - Get or set the Define components of vector v for AOPT = 3 and 4
          * - :py:attr:`~v2`
            - Get or set the Define components of vector v for AOPT = 3 and 4
          * - :py:attr:`~v3`
            - Get or set the Define components of vector v for AOPT = 3 and 4
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

    from mat_honeycomb import MatHoneycomb

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


   
   Get or set the Young's modulus for compacted honeycomb material.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio for compacted honeycomb material.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Yield stress for fully compacted honeycomb.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf
   :type: Optional[float]


   
   Get or set the Relative volume at which the honeycomb is fully compacted.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: float


   
   Get or set the Material viscosity coefficient (default=.05) Recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: bulk
   :type: float


   
   Get or set the Bulk viscosity flag:
   EQ.0.0: bulk viscosity is not used. This is recommended.
   EQ.1.0: bulk viscosity is active and MU=0. This will give results identical to previous versions of LS-DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: lca
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-aa versus either relative volume or volumetric strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcb
   :type: int


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-bb versus either relative volume or volumetric strain (default LCB=LCA).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcc
   :type: int


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-bb versus either relative volume or volumetric strain (default LCC=LCA).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcs
   :type: int


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for shear stress versus either relative volume or volumetric strain (default LCS=LCA).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcab
   :type: int


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-ab versus either relative volume or volumetric strain (default LCAB=LCS).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcbc
   :type: int


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for sigma-bc versus either relative volume or volumetric strain (default LCBC=LCS).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcca
   :type: int


   
   Get or set the Load curve ID, see *DEFINE_CURVE, or sigma-ca versus either relative volume or volumetric strain (default LCCA=LCS).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: int


   
   Get or set the Load curve ID, see *DEFINE_CURVE, for strain-rate effects defining the scale factor versus strain rate (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: eaau
   :type: Optional[float]


   
   Get or set the Elastic modulus Eaau in uncompressed configuration.
















   ..
       !! processed by numpydoc !!

.. py:property:: ebbu
   :type: Optional[float]


   
   Get or set the Elastic modulus Ebbu in uncompressed configuration.
















   ..
       !! processed by numpydoc !!

.. py:property:: eccu
   :type: Optional[float]


   
   Get or set the Elastic modulus Eccu in uncompressed configuration.
















   ..
       !! processed by numpydoc !!

.. py:property:: gabu
   :type: Optional[float]


   
   Get or set the Shear modulus Gabu in uncompressed configuration.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbcu
   :type: Optional[float]


   
   Get or set the Shear modulus Gbcu in uncompressed configuration.
















   ..
       !! processed by numpydoc !!

.. py:property:: gcau
   :type: Optional[float]


   
   Get or set the Shear modulus Gcau in uncompressed configuration.
















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

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinates of point p for AOPT = 1.
















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

.. py:property:: tsef
   :type: Optional[float]


   
   Get or set the Tensile strain at element failure (element will erode).
















   ..
       !! processed by numpydoc !!

.. py:property:: ssef
   :type: Optional[float]


   
   Get or set the Shear strain at element failure (element will erode).
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define components of vector v for AOPT = 3 and 4
















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
   :value: 'HONEYCOMB'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





