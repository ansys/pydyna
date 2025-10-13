





:class:`DefineSphMeshBox`
=========================


.. py:class:: define_sph_mesh_box.DefineSphMeshBox(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPH_MESH_BOX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSphMeshBox

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~xmin`
            - Get or set the Minimum x-coordinate
          * - :py:attr:`~ymin`
            - Get or set the Minimum y-coordinate
          * - :py:attr:`~zmin`
            - Get or set the Minimum z-coordinate
          * - :py:attr:`~xlen`
            - Get or set the Box length in the x-direction.
          * - :py:attr:`~ylen`
            - Get or set the Box length in the y-direction.
          * - :py:attr:`~zlen`
            - Get or set the Box length in the z-direction.
          * - :py:attr:`~ipid`
            - Get or set the Part ID for generated SPH elements
          * - :py:attr:`~nx`
            - Get or set the Number of SPH particles in the x-direction.
          * - :py:attr:`~ny`
            - Get or set the Number of SPH particles in the y-direction.
          * - :py:attr:`~nz`
            - Get or set the Number of SPH particles in the z-direction.
          * - :py:attr:`~idseg`
            - Get or set the Segment set ID that can be used to removed generated SPH elements. segment set is used to split the box into two regions, one that has SPH elements and one without SPH (see Remark 2). The sign of IDSEG determines which region keeps the SPH elements. Also, to avoid sudden movement, elements that are "too close" to the segment set will be removed, regardless of the sign of IDSEG. Too close means the normal distance from the center of the SPH element to the nearest segment is smaller than the SPH smoothing length scaled by SFSP.
          * - :py:attr:`~sfsp`
            - Get or set the Scale factor for interparticle distance and only active when IDSEG.ne.0.
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

    from define_sph_mesh_box import DefineSphMeshBox

Property detail
---------------

.. py:property:: xmin
   :type: Optional[float]


   
   Get or set the Minimum x-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: ymin
   :type: Optional[float]


   
   Get or set the Minimum y-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: zmin
   :type: Optional[float]


   
   Get or set the Minimum z-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: xlen
   :type: Optional[float]


   
   Get or set the Box length in the x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ylen
   :type: Optional[float]


   
   Get or set the Box length in the y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: zlen
   :type: Optional[float]


   
   Get or set the Box length in the z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipid
   :type: Optional[int]


   
   Get or set the Part ID for generated SPH elements
















   ..
       !! processed by numpydoc !!

.. py:property:: nx
   :type: Optional[int]


   
   Get or set the Number of SPH particles in the x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ny
   :type: Optional[int]


   
   Get or set the Number of SPH particles in the y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: nz
   :type: Optional[int]


   
   Get or set the Number of SPH particles in the z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: idseg
   :type: int


   
   Get or set the Segment set ID that can be used to removed generated SPH elements. segment set is used to split the box into two regions, one that has SPH elements and one without SPH (see Remark 2). The sign of IDSEG determines which region keeps the SPH elements. Also, to avoid sudden movement, elements that are "too close" to the segment set will be removed, regardless of the sign of IDSEG. Too close means the normal distance from the center of the SPH element to the nearest segment is smaller than the SPH smoothing length scaled by SFSP.
   EQ.0 : No generated elements are removed.
   GT.0 : Keep the SPH element if it lies nominally in the normal direction of the segments in the segment set.
   LT.0 : Keep the SPH element if it lies nominally in the reverse normal direction of segments in the segment set.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfsp
   :type: Optional[float]


   
   Get or set the Scale factor for interparticle distance and only active when IDSEG.ne.0.
   If the distance between SPH particle and nearest segment is smaller than this distance, SPH element is removed.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'SPH_MESH_BOX'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





