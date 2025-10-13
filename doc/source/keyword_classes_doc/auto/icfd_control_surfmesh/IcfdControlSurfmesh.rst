





:class:`IcfdControlSurfmesh`
============================


.. py:class:: icfd_control_surfmesh.IcfdControlSurfmesh(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_SURFMESH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlSurfmesh

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rsrf`
            - Get or set the Indicates whether or not to perform a surface re-meshing.
          * - :py:attr:`~sadapt`
            - Get or set the Indicates whether or not to trigger adaptive surface remeshing.


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

    from icfd_control_surfmesh import IcfdControlSurfmesh

Property detail
---------------

.. py:property:: rsrf
   :type: int


   
   Get or set the Indicates whether or not to perform a surface re-meshing.
   EQ.0: no re-meshing is applied..
   EQ.1: Laplacian smoothing surface remeshing
   EQ.2: Curvature preserving surface remeshing.
















   ..
       !! processed by numpydoc !!

.. py:property:: sadapt
   :type: int


   
   Get or set the Indicates whether or not to trigger adaptive surface remeshing.
   EQ.0: no adaptive surface re-meshing is applied.
   EQ.1: automatic surface remeshing when quality deteriorates (3D only).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_SURFMESH'






