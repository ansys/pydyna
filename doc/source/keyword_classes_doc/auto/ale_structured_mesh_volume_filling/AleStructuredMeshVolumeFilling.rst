





:class:`AleStructuredMeshVolumeFilling`
=======================================


.. py:class:: ale_structured_mesh_volume_filling.AleStructuredMeshVolumeFilling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_STRUCTURED_MESH_VOLUME_FILLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleStructuredMeshVolumeFilling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mshid`
            - Get or set the S-ALE Mesh ID. A unique number must be specified.
          * - :py:attr:`~ammgto`
            - Get or set the The ID of AMMG filling the geometry. See *ALE_MULTI-MATERIAL_GROUP for reference.
          * - :py:attr:`~nsample`
            - Get or set the Number of sampling points.  In case an element is partially filled, in each direction, 2 * NSAMPLE + 1 points are generated.
          * - :py:attr:`~unused_`
            - Get or set the -.
          * - :py:attr:`~vid`
            - Get or set the ID of *DEFINE_VECTOR card.  This flag is used to assign initial velocity to material filling the domain.
          * - :py:attr:`~geom`
            - Get or set the Geometry types. They are: PARTSET, PART, SEGSET, PLANE, CYLINDER, BOXCOR, BOXCPT and SPHERE.
          * - :py:attr:`~in_out`
            - Get or set the To fill inside or outside of the geometry.  For PARTSET‌ / PART / SEGSET options, inside is taken as in the normal direction of the container’s segments (see Remark 1).
          * - :py:attr:`~e1`
            - Get or set the These values have different definitions for different options.
          * - :py:attr:`~e2`
            - Get or set the These values have different definitions for different options.
          * - :py:attr:`~e3`
            - Get or set the These values have different definitions for different options.
          * - :py:attr:`~e4`
            - Get or set the These values have different definitions for different options.
          * - :py:attr:`~e5`
            - Get or set the These values have different definitions for different options.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from ale_structured_mesh_volume_filling import AleStructuredMeshVolumeFilling

Property detail
---------------

.. py:property:: mshid
   :type: int


   
   Get or set the S-ALE Mesh ID. A unique number must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ammgto
   :type: str


   
   Get or set the The ID of AMMG filling the geometry. See *ALE_MULTI-MATERIAL_GROUP for reference.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsample
   :type: int


   
   Get or set the Number of sampling points.  In case an element is partially filled, in each direction, 2 * NSAMPLE + 1 points are generated.
   These (2*"NSAMPLE" +1)^3 points, each representing a volume, are used to determine if its volume is in or out.
















   ..
       !! processed by numpydoc !!

.. py:property:: unused_
   :type: Optional[int]


   
   Get or set the -.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the ID of *DEFINE_VECTOR card.  This flag is used to assign initial velocity to material filling the domain.
   Field 2 to 5 (XT, YT, ZT) of the *DEFINE_VECTOR card are used to define the initial translational velocities.  Please refer to Example 1 below for usage.
















   ..
       !! processed by numpydoc !!

.. py:property:: geom
   :type: str


   
   Get or set the Geometry types. They are: PARTSET, PART, SEGSET, PLANE, CYLINDER, BOXCOR, BOXCPT and SPHERE.
   See the table below for more details.
















   ..
       !! processed by numpydoc !!

.. py:property:: in_out
   :type: int


   
   Get or set the To fill inside or outside of the geometry.  For PARTSET‌ / PART / SEGSET options, inside is taken as in the normal direction of the container’s segments (see Remark 1).
   EQ.0:   Inside(default)
   EQ.1 : Outside
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[float]


   
   Get or set the These values have different definitions for different options.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[float]


   
   Get or set the These values have different definitions for different options.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[float]


   
   Get or set the These values have different definitions for different options.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[float]


   
   Get or set the These values have different definitions for different options.
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[float]


   
   Get or set the These values have different definitions for different options.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'STRUCTURED_MESH_VOLUME_FILLING'






