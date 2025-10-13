





:class:`DualceseBoundaryFsiMsurf`
=================================


.. py:class:: dualcese_boundary_fsi_msurf.DualceseBoundaryFsiMsurf(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_BOUNDARY_FSI_MSURF keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseBoundaryFsiMsurf

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mspid`
            - Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
          * - :py:attr:`~ref_p`
            - Get or set the Ambient/reference pressure of the fluid domain on the side opposite this structural interface to the fluid simulation domain.  This ambient pressure only needs to be specified in the case where the FSI structural part(s) connected with this FSI interface are not immersed in the dual CESE mesh.  This reference pressure defaults to 0.0 since moving mesh FSI calculations most often involve structures surrounded by the dual CESE mesh, and there is no need for a reference pressure in that case


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

    from dualcese_boundary_fsi_msurf import DualceseBoundaryFsiMsurf

Property detail
---------------

.. py:property:: mspid
   :type: Optional[int]


   
   Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
















   ..
       !! processed by numpydoc !!

.. py:property:: ref_p
   :type: float


   
   Get or set the Ambient/reference pressure of the fluid domain on the side opposite this structural interface to the fluid simulation domain.  This ambient pressure only needs to be specified in the case where the FSI structural part(s) connected with this FSI interface are not immersed in the dual CESE mesh.  This reference pressure defaults to 0.0 since moving mesh FSI calculations most often involve structures surrounded by the dual CESE mesh, and there is no need for a reference pressure in that case
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_FSI_MSURF'






