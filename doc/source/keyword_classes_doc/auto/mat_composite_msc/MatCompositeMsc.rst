





:class:`MatCompositeMsc`
========================


.. py:class:: mat_composite_msc.MatCompositeMsc(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_COMPOSITE_MSC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCompositeMsc

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
            - Get or set the Ea, Young's modulus - longitudinal direction
          * - :py:attr:`~eb`
            - Get or set the Eb, Young's modulus - transverse direction
          * - :py:attr:`~ec`
            - Get or set the Ec, Young's modulus - through thickness direction
          * - :py:attr:`~prba`
            - Get or set the Vba , Poisson's ratio ba
          * - :py:attr:`~prca`
            - Get or set the Vca , Poisson's ratio ca
          * - :py:attr:`~prcb`
            - Get or set the Vab , Poisson's ratio cb
          * - :py:attr:`~gab`
            - Get or set the Gab, shear modulus ab
          * - :py:attr:`~gbc`
            - Get or set the Gbc, shear modulus bc
          * - :py:attr:`~gca`
            - Get or set the Gca, shear modulus ca
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the Define X coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the Define Y coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the Define Z coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the Define x-component of vector A for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Define y-component of vector A for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Define z-component of vector A for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Define x-component of vector V for AOPT = 2.
          * - :py:attr:`~v2`
            - Get or set the Define y-component of vector V for AOPT = 2.
          * - :py:attr:`~v3`
            - Get or set the Define z-component of vector V for AOPT = 2.
          * - :py:attr:`~d1`
            - Get or set the Define x-component of vector D for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Define y-component of vector D for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Define z-component of vector D for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Layer in-plane rotational angle in degrees.
          * - :py:attr:`~sat`
            - Get or set the Longitudinal tensile strength
          * - :py:attr:`~sac`
            - Get or set the Longitudinal compressive strength
          * - :py:attr:`~sbt`
            - Get or set the Transverse tensile strength
          * - :py:attr:`~sbc`
            - Get or set the Transverse compressive strength
          * - :py:attr:`~sct`
            - Get or set the Through thickness tensile strength
          * - :py:attr:`~sfc`
            - Get or set the Crush strength
          * - :py:attr:`~sfs`
            - Get or set the Fiber mode shear strength
          * - :py:attr:`~sab`
            - Get or set the Matrix mode shear strength, ab plane, see below.
          * - :py:attr:`~sca`
            - Get or set the Matrix mode shear strength, ca plane, see below.
          * - :py:attr:`~sffc`
            - Get or set the Scale factor for residual compressive strength
          * - :py:attr:`~amodel`
            - Get or set the Material models:
          * - :py:attr:`~phic`
            - Get or set the Coulomb friction angle for matrix and delamination failure
          * - :py:attr:`~e_limt`
            - Get or set the Element eroding axial strain
          * - :py:attr:`~s_delm`
            - Get or set the Scale factor for delamination criterion
          * - :py:attr:`~omgmx`
            - Get or set the Limit damage parameter for elastic modulus reduction
          * - :py:attr:`~ecrsh`
            - Get or set the Limit compressive volume strain for element eroding
          * - :py:attr:`~eexpn`
            - Get or set the Limit tensile volume strain for element eroding
          * - :py:attr:`~cerate1`
            - Get or set the Coefficient for strain rate dependent strength properties.
          * - :py:attr:`~am1`
            - Get or set the Coefficient for strain rate softening property for fiber damage in a direction.
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

    from mat_composite_msc import MatCompositeMsc

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


   
   Get or set the Ea, Young's modulus - longitudinal direction
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Eb, Young's modulus - transverse direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Ec, Young's modulus - through thickness direction
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Vba , Poisson's ratio ba
















   ..
       !! processed by numpydoc !!

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the Vca , Poisson's ratio ca
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Vab , Poisson's ratio cb
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Gab, shear modulus ab
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the Gbc, shear modulus bc
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the Gca, shear modulus ca
















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


   
   Get or set the Define X coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Define Y coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Define Z coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Define x-component of vector A for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Define y-component of vector A for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Define z-component of vector A for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Define x-component of vector V for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Define y-component of vector V for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Define z-component of vector V for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Define x-component of vector D for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Define y-component of vector D for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Define z-component of vector D for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Layer in-plane rotational angle in degrees.
















   ..
       !! processed by numpydoc !!

.. py:property:: sat
   :type: Optional[float]


   
   Get or set the Longitudinal tensile strength
















   ..
       !! processed by numpydoc !!

.. py:property:: sac
   :type: Optional[float]


   
   Get or set the Longitudinal compressive strength
















   ..
       !! processed by numpydoc !!

.. py:property:: sbt
   :type: Optional[float]


   
   Get or set the Transverse tensile strength
















   ..
       !! processed by numpydoc !!

.. py:property:: sbc
   :type: Optional[float]


   
   Get or set the Transverse compressive strength
















   ..
       !! processed by numpydoc !!

.. py:property:: sct
   :type: Optional[float]


   
   Get or set the Through thickness tensile strength
















   ..
       !! processed by numpydoc !!

.. py:property:: sfc
   :type: Optional[float]


   
   Get or set the Crush strength
















   ..
       !! processed by numpydoc !!

.. py:property:: sfs
   :type: Optional[float]


   
   Get or set the Fiber mode shear strength
















   ..
       !! processed by numpydoc !!

.. py:property:: sab
   :type: Optional[float]


   
   Get or set the Matrix mode shear strength, ab plane, see below.
















   ..
       !! processed by numpydoc !!

.. py:property:: sca
   :type: Optional[float]


   
   Get or set the Matrix mode shear strength, ca plane, see below.
















   ..
       !! processed by numpydoc !!

.. py:property:: sffc
   :type: Optional[float]


   
   Get or set the Scale factor for residual compressive strength
















   ..
       !! processed by numpydoc !!

.. py:property:: amodel
   :type: int


   
   Get or set the Material models:
   EQ. 1: Unidirectional layer model
   EQ. 2: Fabric layer model
















   ..
       !! processed by numpydoc !!

.. py:property:: phic
   :type: Optional[float]


   
   Get or set the Coulomb friction angle for matrix and delamination failure
















   ..
       !! processed by numpydoc !!

.. py:property:: e_limt
   :type: Optional[float]


   
   Get or set the Element eroding axial strain
















   ..
       !! processed by numpydoc !!

.. py:property:: s_delm
   :type: Optional[float]


   
   Get or set the Scale factor for delamination criterion
















   ..
       !! processed by numpydoc !!

.. py:property:: omgmx
   :type: Optional[float]


   
   Get or set the Limit damage parameter for elastic modulus reduction
















   ..
       !! processed by numpydoc !!

.. py:property:: ecrsh
   :type: Optional[float]


   
   Get or set the Limit compressive volume strain for element eroding
















   ..
       !! processed by numpydoc !!

.. py:property:: eexpn
   :type: Optional[float]


   
   Get or set the Limit tensile volume strain for element eroding
















   ..
       !! processed by numpydoc !!

.. py:property:: cerate1
   :type: Optional[float]


   
   Get or set the Coefficient for strain rate dependent strength properties.
















   ..
       !! processed by numpydoc !!

.. py:property:: am1
   :type: Optional[float]


   
   Get or set the Coefficient for strain rate softening property for fiber damage in a direction.
















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
   :value: 'COMPOSITE_MSC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





