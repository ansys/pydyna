





:class:`InterfaceEnsight`
=========================


.. py:class:: interface_ensight.InterfaceEnsight(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_ENSIGHT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceEnsight

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nset`
            - Get or set the Set of node in ls-dyna input deck, which contains all nodes to be subject to transient loading mapped from the following Ensight-formatted geometry file and loading files.
          * - :py:attr:`~nlfile`
            - Get or set the number of transient loading files, which contain loading info. For each node defined in the geometry file at various time steps defined in the last card.
          * - :py:attr:`~gfile`
            - Get or set the EnSightGold-formatted geometry file describing part of the mesh, nodal   coordinates and element connectivity, used by other solver to yield the loading described by the following loading files.  Even the geometry file can contain the whole mesh or just part of the mesh subject to transient loading defined in the following transient loading, the latter is recommended �C including only mesh subject to loading defined in   loading files �C to  reduce memory requirement and speed up analysis.


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

    from interface_ensight import InterfaceEnsight

Property detail
---------------

.. py:property:: nset
   :type: Optional[int]


   
   Get or set the Set of node in ls-dyna input deck, which contains all nodes to be subject to transient loading mapped from the following Ensight-formatted geometry file and loading files.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlfile
   :type: Optional[int]


   
   Get or set the number of transient loading files, which contain loading info. For each node defined in the geometry file at various time steps defined in the last card.
















   ..
       !! processed by numpydoc !!

.. py:property:: gfile
   :type: Optional[str]


   
   Get or set the EnSightGold-formatted geometry file describing part of the mesh, nodal   coordinates and element connectivity, used by other solver to yield the loading described by the following loading files.  Even the geometry file can contain the whole mesh or just part of the mesh subject to transient loading defined in the following transient loading, the latter is recommended �C including only mesh subject to loading defined in   loading files �C to  reduce memory requirement and speed up analysis.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'ENSIGHT'






