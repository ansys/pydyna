





:class:`InitialDetonationGeometry`
==================================


.. py:class:: initial_detonation_geometry.InitialDetonationGeometry(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_DETONATION_GEOMETRY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialDetonationGeometry

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~heid`
            - Get or set the ID specifying the high explosives to be lit
          * - :py:attr:`~hetyp`
            - Get or set the Type of HEID :
          * - :py:attr:`~mmgse`
            - Get or set the ID of *SET_‌MULTI-MATERIAL_‌GROUP_LIST selecting the explosive ALE groups to be lit
          * - :py:attr:`~geotyp`
            - Get or set the Type of geometry formed by the detonation points:
          * - :py:attr:`~lt`
            - Get or set the Lighting time for detonation point
          * - :py:attr:`~dgeo`
            - Get or set the Maximum distance from the detonation geometry for determining which HE elements become detonation points. If the element center for the specified HE is less than this distance away from the detonation geometry, then the element center becomes detonation point. If zero or undefined, DGEO becomes the length of the largest specified HE element.
          * - :py:attr:`~v1`
            - Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
          * - :py:attr:`~v2`
            - Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
          * - :py:attr:`~v3`
            - Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
          * - :py:attr:`~v4`
            - Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.


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

    from initial_detonation_geometry import InitialDetonationGeometry

Property detail
---------------

.. py:property:: heid
   :type: Optional[int]


   
   Get or set the ID specifying the high explosives to be lit
















   ..
       !! processed by numpydoc !!

.. py:property:: hetyp
   :type: float


   
   Get or set the Type of HEID :
   EQ.0:   Part set(*SET_PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgse
   :type: float


   
   Get or set the ID of *SET_‌MULTI-MATERIAL_‌GROUP_LIST selecting the explosive ALE groups to be lit
















   ..
       !! processed by numpydoc !!

.. py:property:: geotyp
   :type: int


   
   Get or set the Type of geometry formed by the detonation points:
   EQ.1: Plane
















   ..
       !! processed by numpydoc !!

.. py:property:: lt
   :type: float


   
   Get or set the Lighting time for detonation point
















   ..
       !! processed by numpydoc !!

.. py:property:: dgeo
   :type: float


   
   Get or set the Maximum distance from the detonation geometry for determining which HE elements become detonation points. If the element center for the specified HE is less than this distance away from the detonation geometry, then the element center becomes detonation point. If zero or undefined, DGEO becomes the length of the largest specified HE element.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[int]


   
   Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
   GEOTYP.EQ.1:    V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[int]


   
   Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
   GEOTYP.EQ.1:    V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[int]


   
   Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
   GEOTYP.EQ.1:    V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
















   ..
       !! processed by numpydoc !!

.. py:property:: v4
   :type: Optional[int]


   
   Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
   GEOTYP.EQ.1:    V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'DETONATION_GEOMETRY'






