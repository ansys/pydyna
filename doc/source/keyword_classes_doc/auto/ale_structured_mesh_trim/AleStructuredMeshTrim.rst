





:class:`AleStructuredMeshTrim`
==============================


.. py:class:: ale_structured_mesh_trim.AleStructuredMeshTrim(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_STRUCTURED_MESH_TRIM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleStructuredMeshTrim

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mshid`
            - Get or set the S-ALE Mesh ID. The ID of the Structured ALE mesh to be trimed/un-trimed.
          * - :py:attr:`~option`
            - Get or set the There are six available options. They are trim by: PARTSET, SEGSET, PLANE,
          * - :py:attr:`~oper`
            - Get or set the To trim or un-trim, that is, to delete the picked elements or keep them.
          * - :py:attr:`~ioutin`
            - Get or set the Flag to select which elements to trim, that is, "outside" or "inside" the specified object defined with the OPTION and En.
          * - :py:attr:`~psid`
            - Get or set the shell part set ID.
          * - :py:attr:`~dist`
            - Get or set the the distance.  Elements farther away than the distance in the direction of the shell normal vectors (depending on the value of IOUTIN) are deleted/kept.
          * - :py:attr:`~e3`
            - Get or set the -.
          * - :py:attr:`~e4`
            - Get or set the -.


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

    from ale_structured_mesh_trim import AleStructuredMeshTrim

Property detail
---------------

.. py:property:: mshid
   :type: int


   
   Get or set the S-ALE Mesh ID. The ID of the Structured ALE mesh to be trimed/un-trimed.
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the There are six available options. They are trim by: PARTSET, SEGSET, PLANE,
   CYLINDER, BOXCOR, BOXCPT and SPHERE.  See the table below for more details.
















   ..
       !! processed by numpydoc !!

.. py:property:: oper
   :type: int


   
   Get or set the To trim or un-trim, that is, to delete the picked elements or keep them.
   EQ.0:   trim (default)
   EQ.1:   keep.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioutin
   :type: int


   
   Get or set the Flag to select which elements to trim, that is, "outside" or "inside" the specified object defined with the OPTION and En.
   For PARTSET and SEGSET options, "outside" is defined as the region to which the segment normal points.
   EQ.0:   outside (default)
   EQ.1:   inside.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the shell part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dist
   :type: Optional[float]


   
   Get or set the the distance.  Elements farther away than the distance in the direction of the shell normal vectors (depending on the value of IOUTIN) are deleted/kept.
   Please note, only elements on one side will be deleted.
   To delete the elements on both sides, repeat the card with the IOUTIN value reversed.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[float]


   
   Get or set the -.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[float]


   
   Get or set the -.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'STRUCTURED_MESH_TRIM'






