





:class:`MatHeartTissue`
=======================


.. py:class:: mat_heart_tissue.MatHeartTissue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_HEART_TISSUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatHeartTissue

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
          * - :py:attr:`~c`
            - Get or set the Material coefficient.
          * - :py:attr:`~b1`
            - Get or set the b1 ,material coefficient.
          * - :py:attr:`~b2`
            - Get or set the b1 ,material coefficient.
          * - :py:attr:`~b3`
            - Get or set the b1 ,material coefficient.
          * - :py:attr:`~p`
            - Get or set the Pressure in the muscle tissue.
          * - :py:attr:`~b`
            - Get or set the Systolic material coefficient. Omit for the earlier model.
          * - :py:attr:`~l0`
            - Get or set the L0 , sacromere length at which no active tension develops. Omit for the earlier model.
          * - :py:attr:`~ca0max`
            - Get or set the (CA0)max, maximum peak intracellular calcium concentrate. Omit for the earlier model.
          * - :py:attr:`~lr`
            - Get or set the LR , Stress-free sacromere length. Omit for the earlier model.
          * - :py:attr:`~mm`
            - Get or set the Systolic material coefficient. Omit for the earlier model.
          * - :py:attr:`~bb`
            - Get or set the Systolic material coefficient. Omit for the earlier model.
          * - :py:attr:`~ca0`
            - Get or set the CA0, peak intracellular calcium concentration. Omit for the earlier model.
          * - :py:attr:`~tmax`
            - Get or set the Tmax , maximum isometric tension achieved at the longest sacromere length. Omit for the earlier model.
          * - :py:attr:`~tact`
            - Get or set the Tact , time at which active contraction initiates. Omit for the earlier model.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~xp`
            - Get or set the x-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~yp`
            - Get or set the y-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~zp`
            - Get or set the z-coordinate of point p for AOPT = 1 and 4.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3 and 4.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
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

    from mat_heart_tissue import MatHeartTissue

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

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Material coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the b1 ,material coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the b1 ,material coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: b3
   :type: Optional[float]


   
   Get or set the b1 ,material coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Pressure in the muscle tissue.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Systolic material coefficient. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: l0
   :type: Optional[float]


   
   Get or set the L0 , sacromere length at which no active tension develops. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: ca0max
   :type: Optional[int]


   
   Get or set the (CA0)max, maximum peak intracellular calcium concentrate. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: lr
   :type: Optional[float]


   
   Get or set the LR , Stress-free sacromere length. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: mm
   :type: Optional[float]


   
   Get or set the Systolic material coefficient. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: bb
   :type: Optional[float]


   
   Get or set the Systolic material coefficient. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: ca0
   :type: Optional[float]


   
   Get or set the CA0, peak intracellular calcium concentration. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmax
   :type: Optional[float]


   
   Get or set the Tmax , maximum isometric tension achieved at the longest sacromere length. Omit for the earlier model.
















   ..
       !! processed by numpydoc !!

.. py:property:: tact
   :type: Optional[float]


   
   Get or set the Tact , time at which active contraction initiates. Omit for the earlier model.
















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
   Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 5 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinate of point p for AOPT = 1 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinate of point p for AOPT = 1 and 4.
















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

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3 and 4.
















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

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, which may be overridden on the element card, see *ELEMENT_SHELL.
















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
   :value: 'HEART_TISSUE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





