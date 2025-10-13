





:class:`DatabaseHistoryNodeSetLocal`
====================================


.. py:class:: database_history_node_set_local.DatabaseHistoryNodeSetLocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_HISTORY_NODE_SET_LOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseHistoryNodeSetLocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Node set ID. The contents of the files are given in Table 9.1 in the Keyword Manual section 9.14 for nodes.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for nodal output. See *DEFINE_COORDINATE options.
          * - :py:attr:`~ref`
            - Get or set the Output reference:
          * - :py:attr:`~hfo`
            - Get or set the Flag for high frequency output into NODOUTHF.


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

    from database_history_node_set_local import DatabaseHistoryNodeSetLocal

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Node set ID. The contents of the files are given in Table 9.1 in the Keyword Manual section 9.14 for nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for nodal output. See *DEFINE_COORDINATE options.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: int


   
   Get or set the Output reference:
   EQ.0: Output is in the local system fixed for all time from the beginning of the calculation (default),
   EQ.1: Output is in the local system which is defined by the *DEFINE_COORDINATE_NODES. The local system can change orientation depending on the movement of the three defining nodes. The defining nodes can belong to either deformable or rigid parts,
   EQ.2: Output is relative to the local system which is defined by the *DEFINE_COORDINATE_NODES option. The local system can change orientation depending on the movement of the three defining nodes. If dynamic relaxation is used, the reference location is reset when convergence is achieved.
















   ..
       !! processed by numpydoc !!

.. py:property:: hfo
   :type: int


   
   Get or set the Flag for high frequency output into NODOUTHF.
   EQ.0: Nodal data written to NODOUT file only,
   EQ.1: Nodal data also written to NODOUTHF at the higher frequency.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'HISTORY_NODE_SET_LOCAL'






