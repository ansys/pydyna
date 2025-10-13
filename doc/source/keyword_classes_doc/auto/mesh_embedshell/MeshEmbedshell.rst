





:class:`MeshEmbedshell`
=======================


.. py:class:: mesh_embedshell.MeshEmbedshell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MESH_EMBEDSHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MeshEmbedshell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~volid`
            - Get or set the ID assigned to the new volume in the keyword *MESH_VOLUME. The size meshes will be applied to this volume
          * - :py:attr:`~elements`
            - dynamic array of surface element ids..


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

    from mesh_embedshell import MeshEmbedshell

Property detail
---------------

.. py:property:: volid
   :type: Optional[int]


   
   Get or set the ID assigned to the new volume in the keyword *MESH_VOLUME. The size meshes will be applied to this volume
















   ..
       !! processed by numpydoc !!

.. py:property:: elements
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of surface element ids..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MESH'


.. py:attribute:: subkeyword
   :value: 'EMBEDSHELL'






