





:class:`BoundarySaleMeshFace`
=============================


.. py:class:: boundary_sale_mesh_face.BoundarySaleMeshFace(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SALE_MESH_FACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySaleMeshFace

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~option`
            - Get or set the There are 3 options.
          * - :py:attr:`~mshid`
            - Get or set the S-ALE Mesh ID


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

    from boundary_sale_mesh_face import BoundarySaleMeshFace

Property detail
---------------

.. py:property:: option
   :type: str


   
   Get or set the There are 3 options.
   FIXED: All nodes at the face are fixed at all directions
   NOFLOW : No flow allowed through the face
   SYMM : The face is a symmetric plane(same as NOFLOW)
   NONREFL : Non - reflective boundary condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: mshid
   :type: Optional[int]


   
   Get or set the S-ALE Mesh ID
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SALE_MESH_FACE'






