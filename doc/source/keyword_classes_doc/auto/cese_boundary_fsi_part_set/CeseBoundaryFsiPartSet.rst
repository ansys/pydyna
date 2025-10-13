





:class:`CeseBoundaryFsiPartSet`
===============================


.. py:class:: cese_boundary_fsi_part_set.CeseBoundaryFsiPartSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_FSI_PART_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundaryFsiPartSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfsid`
            - Get or set the Identifier of a set of surface part IDs created with a *LSO_ID_SET card, where each surface part ID in the set is referenced in *MESH_SURFACE_ELEMENT cards.


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

    from cese_boundary_fsi_part_set import CeseBoundaryFsiPartSet

Property detail
---------------

.. py:property:: surfsid
   :type: Optional[int]


   
   Get or set the Identifier of a set of surface part IDs created with a *LSO_ID_SET card, where each surface part ID in the set is referenced in *MESH_SURFACE_ELEMENT cards.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_FSI_PART_SET'






