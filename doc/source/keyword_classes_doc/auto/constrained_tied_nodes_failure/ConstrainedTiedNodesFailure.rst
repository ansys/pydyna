





:class:`ConstrainedTiedNodesFailure`
====================================


.. py:class:: constrained_tied_nodes_failure.ConstrainedTiedNodesFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_TIED_NODES_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedTiedNodesFailure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Node set ID, see *SET_NODE.
          * - :py:attr:`~eppf`
            - Get or set the Plastic strain, volumetric strain, or damage (MAT_107, MAT_110, MAT_224, or GISSMO) at failure.
          * - :py:attr:`~etype`
            - Get or set the Element type for nodal group:


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

    from constrained_tied_nodes_failure import ConstrainedTiedNodesFailure

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: eppf
   :type: float


   
   Get or set the Plastic strain, volumetric strain, or damage (MAT_107, MAT_110, MAT_224, or GISSMO) at failure.
















   ..
       !! processed by numpydoc !!

.. py:property:: etype
   :type: int


   
   Get or set the Element type for nodal group:
   EQ:0: shell,
   EQ.1: solid element.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'TIED_NODES_FAILURE'






