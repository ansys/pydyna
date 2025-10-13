





:class:`MatUserDefinedMaterialModels`
=====================================


.. py:class:: mat_user_defined_material_models.MatUserDefinedMaterialModels(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_USER_DEFINED_MATERIAL_MODELS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatUserDefinedMaterialModels

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
          * - :py:attr:`~mt`
            - Get or set the User material type (41-50 inclusive). A number between 41 and 50 has to be chosen.
          * - :py:attr:`~lmc`
            - Get or set the Length of material constant array which is equal to the number of material constants to be input.
          * - :py:attr:`~nhv`
            - Get or set the Number of history variables to be stored, see also Appendix A of users manual.
          * - :py:attr:`~iortho`
            - Get or set the Orthotropic/spot weld thinning flag:
          * - :py:attr:`~ibulk`
            - Get or set the Address of bulk modulus in material constants array, see also Appendix A of users manual.
          * - :py:attr:`~ig`
            - Get or set the Address of shear modulus in material constants array, see also Appendix A of users manual..
          * - :py:attr:`~ivect`
            - Get or set the Vectorization flag:EQ.0: off (default),
          * - :py:attr:`~ifail`
            - Get or set the Failure flag.
          * - :py:attr:`~itherm`
            - Get or set the Failure flag (on=1). Compute element temperature.
          * - :py:attr:`~ihyper`
            - Get or set the IHYPER
          * - :py:attr:`~ieos`
            - Get or set the IEOS
          * - :py:attr:`~lmca`
            - Get or set the Length of additional material constant array
          * - :py:attr:`~aopt`
            - Get or set the Material axes option:
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for brick elements for quick changes: EQ.1: default,
          * - :py:attr:`~xp`
            - Get or set the x-coordinate of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the y-coordinate of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the z-coordinate of point p for AOPT = 1.
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
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
          * - :py:attr:`~ievts`
            - Get or set the Address of E(a) for ortho tropic material in thick shell formulation 5 (see remark 6).
          * - :py:attr:`~p1`
            - Get or set the First material parameter.
          * - :py:attr:`~p2`
            - Get or set the Second material parameter.
          * - :py:attr:`~p3`
            - Get or set the Third material parameter.
          * - :py:attr:`~p4`
            - Get or set the Fourth material parameter.
          * - :py:attr:`~p5`
            - Get or set the Fifth material parameter.
          * - :py:attr:`~p6`
            - Get or set the Sixth material parameter.
          * - :py:attr:`~p7`
            - Get or set the Seventh material parameter.
          * - :py:attr:`~p8`
            - Get or set the Eighth material parameter.
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

    from mat_user_defined_material_models import MatUserDefinedMaterialModels

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

.. py:property:: mt
   :type: Optional[int]


   
   Get or set the User material type (41-50 inclusive). A number between 41 and 50 has to be chosen.
   If MT < 0, subroutine rwumat in dyn21.f is called, where the material parameter reading can be modified
   WARNING: If two or more materials in an input deck
   share the same MT value, those materials also share values of other variables on Cards 1 and 2 excluding
   MID and RO. Those shared values are taken from the first material where the common MT is encountered.
















   ..
       !! processed by numpydoc !!

.. py:property:: lmc
   :type: Optional[int]


   
   Get or set the Length of material constant array which is equal to the number of material constants to be input.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhv
   :type: Optional[int]


   
   Get or set the Number of history variables to be stored, see also Appendix A of users manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: iortho
   :type: int


   
   Get or set the Orthotropic/spot weld thinning flag:
   EQ.0:   if the material is not orthotropic and is not used with spot weld thinning
   EQ.1:   if the material is orthotropic
   EQ.2:   if material is used with spot weld thinning
   EQ.3:   if material is orthotropic and used with spot weld thinning.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibulk
   :type: Optional[int]


   
   Get or set the Address of bulk modulus in material constants array, see also Appendix A of users manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: ig
   :type: Optional[int]


   
   Get or set the Address of shear modulus in material constants array, see also Appendix A of users manual..
















   ..
       !! processed by numpydoc !!

.. py:property:: ivect
   :type: int


   
   Get or set the Vectorization flag:EQ.0: off (default),
   EQ.1 on.
   A vectorized user subroutine must be supplied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifail
   :type: Optional[int]


   
   Get or set the Failure flag.
   EQ.0:  No failure;
   EQ.1:  Allows failure of shell and solid elements;
   LT.0:  |IFAIL| is the address of NUMINT in the material constants array.  NUMINT is defined as the number of failed integration points that will trigger element deletion.  This option applies only to shell and solid elements (release 5 of v.971).
















   ..
       !! processed by numpydoc !!

.. py:property:: itherm
   :type: int


   
   Get or set the Failure flag (on=1). Compute element temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihyper
   :type: int


   
   Get or set the IHYPER
















   ..
       !! processed by numpydoc !!

.. py:property:: ieos
   :type: int


   
   Get or set the IEOS
















   ..
       !! processed by numpydoc !!

.. py:property:: lmca
   :type: Optional[int]


   
   Get or set the Length of additional material constant array
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option:
   EQ.0.0: locally orthotropic with material axes determined by element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES,
   and then, for shells only, rotated about        the shell element normal by an angle BETA,
   EQ.1.0: locally orthotropic with material axes determined by a point in space and the global location of the element center, this is the a-direction.
   This option is for solid elements only.
   EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDINATE_VECTOR,
   EQ.3.0:  locally orthotropic material axes determined by rotating
   the material axes about the element normal by an angle,
   BETA, from a line in the plane of the element defined by        the cross product of the vector v with the element normal.
   EQ.4.0: locally orthotropic in cylindrical coordinate system with
   the material axes determined by a vector v, and an originating point, p, which define  the centerline axis. This option is for solid elements only
   LT.0.0: the absolute value of AOPT is the coordinate system ID number (CID on *DEFINE_COORDINATE_NODES, _SYSTEM or _VECTOR). Available in R3 version of 971 and later
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for brick elements for quick changes: EQ.1: default,
   EQ.2: switch material axes a and b,
   EQ.3: switch material axes a and c.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinate of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinate of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinate of point p for AOPT = 1.
















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


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
















   ..
       !! processed by numpydoc !!

.. py:property:: ievts
   :type: Optional[int]


   
   Get or set the Address of E(a) for ortho tropic material in thick shell formulation 5 (see remark 6).
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the First material parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Second material parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Third material parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Fourth material parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Fifth material parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Sixth material parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p7
   :type: Optional[float]


   
   Get or set the Seventh material parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: p8
   :type: Optional[float]


   
   Get or set the Eighth material parameter.
















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
   :value: 'USER_DEFINED_MATERIAL_MODELS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





