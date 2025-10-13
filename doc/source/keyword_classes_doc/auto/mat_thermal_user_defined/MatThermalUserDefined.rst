





:class:`MatThermalUserDefined`
==============================


.. py:class:: mat_thermal_user_defined.MatThermalUserDefined(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_THERMAL_USER_DEFINED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatThermalUserDefined

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tmid`
            - Get or set the Thermal material identification. A unique number has to be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~mt`
            - Get or set the User material type (11-15 inclusive). A number between 11 and 15 has to be chosen.
          * - :py:attr:`~lmc`
            - Get or set the Length of material constant array which is equal to the number of material constants to be input.
          * - :py:attr:`~nhv`
            - Get or set the Number of history variables to be stored, see also Appendix A of users manual.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option of orthotropic materials (see MAT_OPTIONTROPIC_ELASTIC for more details). Set if IORTHO = 1.0.
          * - :py:attr:`~iortho`
            - Get or set the Orthtropic flag:
          * - :py:attr:`~ihve`
            - Get or set the Address of bulk modulus in material constants array, see also Appendix A of users manual.
          * - :py:attr:`~xp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4
          * - :py:attr:`~yp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4
          * - :py:attr:`~zp`
            - Get or set the Coordinates of point p for AOPT = 1 and 4
          * - :py:attr:`~a1`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a2`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~a3`
            - Get or set the Components of vector a for AOPT = 2
          * - :py:attr:`~d1`
            - Get or set the Components of vector d for AOPT = 2, 3 and 4.
          * - :py:attr:`~d2`
            - Get or set the Components of vector d for AOPT = 2, 3 and 4.
          * - :py:attr:`~d3`
            - Get or set the Components of vector d for AOPT = 2, 3 and 4
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

    from mat_thermal_user_defined import MatThermalUserDefined

Property detail
---------------

.. py:property:: tmid
   :type: Optional[int]


   
   Get or set the Thermal material identification. A unique number has to be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: mt
   :type: int


   
   Get or set the User material type (11-15 inclusive). A number between 11 and 15 has to be chosen.
















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

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option of orthotropic materials (see MAT_OPTIONTROPIC_ELASTIC for more details). Set if IORTHO = 1.0.
   EQ.0.0: Locally orthotropic with material axes by element nodes N1, N2and N4
   EQ.1.0 : Locally orthotropic with material axes determined by a point, Image, in spaceand global location of element center
   EQ.2.0 : Globally orthotropic with material axes determined by vectors
   EQ.3.0 : Locally orthotropic with first material axis orthogonal to element normal(defined by element nodes N1, N2 and N4) and to a vector d - Third material direction corresponds to element normal.
   EQ.4.0 : Local orthogonal in cylindrical coordinates with the material axes determined by a vector Image,and an originating point, Image, which define the centerline axis.
   LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR)
















   ..
       !! processed by numpydoc !!

.. py:property:: iortho
   :type: int


   
   Get or set the Orthtropic flag:
   EQ.0: non orthotropic material (default),
   EQ.1: orthotropic material.
















   ..
       !! processed by numpydoc !!

.. py:property:: ihve
   :type: Optional[int]


   
   Get or set the Address of bulk modulus in material constants array, see also Appendix A of users manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Coordinates of point p for AOPT = 1 and 4
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Components of vector a for AOPT = 2
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2, 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2, 3 and 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Components of vector d for AOPT = 2, 3 and 4
















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
   :value: 'THERMAL_USER_DEFINED'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





