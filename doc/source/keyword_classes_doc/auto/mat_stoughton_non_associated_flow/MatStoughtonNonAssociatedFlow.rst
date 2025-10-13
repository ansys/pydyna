





:class:`MatStoughtonNonAssociatedFlow`
======================================


.. py:class:: mat_stoughton_non_associated_flow.MatStoughtonNonAssociatedFlow(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_STOUGHTON_NON_ASSOCIATED_FLOW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatStoughtonNonAssociatedFlow

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's Modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~r00`
            - Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield.
          * - :py:attr:`~r45`
            - Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield..
          * - :py:attr:`~r90`
            - Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield..
          * - :py:attr:`~sig00`
            - Get or set the Initial yield stress from uniaxial tension tests in rolling (0°) direction
          * - :py:attr:`~sig45`
            - Get or set the Initial yield stress from uniaxial tension tests in diagonal (45°) direction
          * - :py:attr:`~sig90`
            - Get or set the Initial yield stress from uniaxial tension tests in transverse (90°) directions
          * - :py:attr:`~sig_b`
            - Get or set the Initial yield stress from equi-biaxial stretching tests
          * - :py:attr:`~lcids`
            - Get or set the ID of a load curve defining stress vs. strain hardening behavior from a uniaxial tension test along the rolling direction.
          * - :py:attr:`~lcidv`
            - Get or set the ID of a load curve defining stress scale factors vs. strain rates; determined from experiments.  An example of the curve can be found in Figure 0-2.  Furthermore, strain rates are stored in history variable #5.  Strain rate scale factors are stored in history variable #6.  To turn on the variables for viewing in LS-PrePost, set NEIPS to at least "6" in *DATABASE_EXTENT_BINARY.  It is very useful to know what levels of strain rates, and strain rate scale factors in a particular simulation.  Once d3plot files are opened in LS-PrePost, individual element time history can be plotted via menu option Post → History, or a color contour of the entire part can be viewed with the menu option Post → FriComp → Misc.
          * - :py:attr:`~scale`
            - Get or set the This variable can be used to speed up the simulation while equalizing the strain rate effect, useful especially in cases where the pulling speed or punch speed is slow.  For example, if the pulling speed is at 15 mm/s but running the simulation at this speed will take a long time, the pulling speed can be increased to 500 mm/s while SCALE can be set to 0.03, giving the same results as those from 15 mm/s, but with the benefit of greatly reduced computational time, see Figures 0-3 and 0-4.  Note the increased absolute value (within a reasonable range) of mass scaling -1.0*dt2ms frequently used in forming simulation does not affect the strain rates, as shown in the Figure 0-5.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2, for shells and solids.
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2, for shells and solids.
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2, for shells and solids.
          * - :py:attr:`~v1`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Components of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2, for solids.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2, for solids.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2, for solids.
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

    from mat_stoughton_non_associated_flow import MatStoughtonNonAssociatedFlow

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















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

.. py:property:: r00
   :type: float


   
   Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield.
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: float


   
   Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield..
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: float


   
   Get or set the Lankford parameters in rolling (0°), diagonal (45°) and transverse (90°) directions, respectively; determined from experiments.  Note if R00, R45, and R90 are not defined or are set to 0.0, then R00 = R45 = R90 = 1.0, which degenerates to the Von-Mises yield..
















   ..
       !! processed by numpydoc !!

.. py:property:: sig00
   :type: Optional[float]


   
   Get or set the Initial yield stress from uniaxial tension tests in rolling (0°) direction
















   ..
       !! processed by numpydoc !!

.. py:property:: sig45
   :type: Optional[float]


   
   Get or set the Initial yield stress from uniaxial tension tests in diagonal (45°) direction
















   ..
       !! processed by numpydoc !!

.. py:property:: sig90
   :type: Optional[float]


   
   Get or set the Initial yield stress from uniaxial tension tests in transverse (90°) directions
















   ..
       !! processed by numpydoc !!

.. py:property:: sig_b
   :type: Optional[float]


   
   Get or set the Initial yield stress from equi-biaxial stretching tests
















   ..
       !! processed by numpydoc !!

.. py:property:: lcids
   :type: Optional[int]


   
   Get or set the ID of a load curve defining stress vs. strain hardening behavior from a uniaxial tension test along the rolling direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidv
   :type: Optional[int]


   
   Get or set the ID of a load curve defining stress scale factors vs. strain rates; determined from experiments.  An example of the curve can be found in Figure 0-2.  Furthermore, strain rates are stored in history variable #5.  Strain rate scale factors are stored in history variable #6.  To turn on the variables for viewing in LS-PrePost, set NEIPS to at least "6" in *DATABASE_EXTENT_BINARY.  It is very useful to know what levels of strain rates, and strain rate scale factors in a particular simulation.  Once d3plot files are opened in LS-PrePost, individual element time history can be plotted via menu option Post → History, or a color contour of the entire part can be viewed with the menu option Post → FriComp → Misc.
















   ..
       !! processed by numpydoc !!

.. py:property:: scale
   :type: float


   
   Get or set the This variable can be used to speed up the simulation while equalizing the strain rate effect, useful especially in cases where the pulling speed or punch speed is slow.  For example, if the pulling speed is at 15 mm/s but running the simulation at this speed will take a long time, the pulling speed can be increased to 500 mm/s while SCALE can be set to 0.03, giving the same results as those from 15 mm/s, but with the benefit of greatly reduced computational time, see Figures 0-3 and 0-4.  Note the increased absolute value (within a reasonable range) of mass scaling -1.0*dt2ms frequently used in forming simulation does not affect the strain rates, as shown in the Figure 0-5.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[int]


   
   Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
   EQ.0.0: Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
   EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
   EQ.2.0: Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
   EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element.
   EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION)
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2, for shells and solids.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2, for shells and solids.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2, for shells and solids.
















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


   
   Get or set the Components of vector d for AOPT = 2, for solids.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2, for solids.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2, for solids.
















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
   :value: 'STOUGHTON_NON_ASSOCIATED_FLOW'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





