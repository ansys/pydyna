





:class:`MeshVolumePart`
=======================


.. py:class:: mesh_volume_part.MeshVolumePart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MESH_VOLUME_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MeshVolumePart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~volprt`
            - Get or set the Part ID of a volume part created by a *MESH_VOLUME card.
          * - :py:attr:`~solprt`
            - Get or set the Part ID of a part created using SOLVERs part card.
          * - :py:attr:`~solver`
            - Get or set the Name of a solver using a mesh created with *MESH cards.


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

    from mesh_volume_part import MeshVolumePart

Property detail
---------------

.. py:property:: volprt
   :type: Optional[int]


   
   Get or set the Part ID of a volume part created by a *MESH_VOLUME card.
















   ..
       !! processed by numpydoc !!

.. py:property:: solprt
   :type: Optional[int]


   
   Get or set the Part ID of a part created using SOLVERs part card.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: Optional[str]


   
   Get or set the Name of a solver using a mesh created with *MESH cards.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MESH'


.. py:attribute:: subkeyword
   :value: 'VOLUME_PART'






