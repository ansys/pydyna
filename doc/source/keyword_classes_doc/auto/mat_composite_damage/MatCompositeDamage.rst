





:class:`MatCompositeDamage`
===========================


.. py:class:: mat_composite_damage.MatCompositeDamage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_COMPOSITE_DAMAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatCompositeDamage

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
            - Get or set the Ea, Young's modulus in a-direction.
          * - :py:attr:`~eb`
            - Get or set the Eb, Young's modulus in b-direction.
          * - :py:attr:`~ec`
            - Get or set the Ec, Young's modulus in c-direction.
          * - :py:attr:`~prba`
            - Get or set the Poisson's ratio, ba.
          * - :py:attr:`~prca`
            - Get or set the Poisson's ratio, ca.
          * - :py:attr:`~prcb`
            - Get or set the Poisson's ratio, cb.
          * - :py:attr:`~gab`
            - Get or set the Shear modulus, ab.
          * - :py:attr:`~gbc`
            - Get or set the Shear modulus, bc.
          * - :py:attr:`~gca`
            - Get or set the Shear modulus, ca.
          * - :py:attr:`~kfail`
            - Get or set the Bulk modulus of failed material. Necessary for compressive failure.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for solid elements:
          * - :py:attr:`~atrack`
            - Get or set the Material a-axis tracking flag (shell elements only)
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
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA, *ELEMENT_TSHELL_BETA, and  *ELEMENT_SOLID_ORTHO.
          * - :py:attr:`~sc`
            - Get or set the Shear strength, ab plane, see Theoretical Manual.
          * - :py:attr:`~xt`
            - Get or set the Longitudinal tensile strength, a-axis, see Theoretical Manual.
          * - :py:attr:`~yt`
            - Get or set the Transverse tensile strength, b-axis, see Theoretical Manual.
          * - :py:attr:`~yc`
            - Get or set the Transverse compressive strength, b-axis, see Theoretical Manual.
          * - :py:attr:`~alph`
            - Get or set the Shear stress parameter for the nonlinear term in units of stress -3, see Theoretical Manual.
          * - :py:attr:`~sn`
            - Get or set the Normal tensile strength (solid elements only).
          * - :py:attr:`~syz`
            - Get or set the Transverse shear strength (solid elements only).
          * - :py:attr:`~szx`
            - Get or set the Transverse shear strength (solid elements only).
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

    from mat_composite_damage import MatCompositeDamage

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


   
   Get or set the Ea, Young's modulus in a-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Eb, Young's modulus in b-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Ec, Young's modulus in c-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Poisson's ratio, ba.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the Poisson's ratio, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Poisson's ratio, cb.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Shear modulus, ab.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the Shear modulus, bc.
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the Shear modulus, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: kfail
   :type: Optional[float]


   
   Get or set the Bulk modulus of failed material. Necessary for compressive failure.
















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

.. py:property:: atrack
   :type: int


   
   Get or set the Material a-axis tracking flag (shell elements only)
   EQ.0: a-axis rotates with element (default)
   EQ.1: a-axis also tracks deformation.
















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

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















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


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA, *ELEMENT_TSHELL_BETA, and  *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc
   :type: Optional[float]


   
   Get or set the Shear strength, ab plane, see Theoretical Manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: xt
   :type: Optional[float]


   
   Get or set the Longitudinal tensile strength, a-axis, see Theoretical Manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: yt
   :type: Optional[float]


   
   Get or set the Transverse tensile strength, b-axis, see Theoretical Manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the Transverse compressive strength, b-axis, see Theoretical Manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: alph
   :type: Optional[float]


   
   Get or set the Shear stress parameter for the nonlinear term in units of stress -3, see Theoretical Manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: sn
   :type: Optional[float]


   
   Get or set the Normal tensile strength (solid elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: syz
   :type: Optional[float]


   
   Get or set the Transverse shear strength (solid elements only).
















   ..
       !! processed by numpydoc !!

.. py:property:: szx
   :type: Optional[float]


   
   Get or set the Transverse shear strength (solid elements only).
















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
   :value: 'COMPOSITE_DAMAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





