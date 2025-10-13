





:class:`InitialHistoryNodeSet`
==============================


.. py:class:: initial_history_node_set.InitialHistoryNodeSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_HISTORY_NODE_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialHistoryNodeSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node id.
          * - :py:attr:`~nhisv`
            - Get or set the Number of history variables to be initialized
          * - :py:attr:`~hindex`
            - Get or set the Define the index in the history variable vector
          * - :py:attr:`~val`
            - Get or set the Define the value of the history variable


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

    from initial_history_node_set import InitialHistoryNodeSet

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node id.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: Optional[int]


   
   Get or set the Number of history variables to be initialized
















   ..
       !! processed by numpydoc !!

.. py:property:: hindex
   :type: Optional[int]


   
   Get or set the Define the index in the history variable vector
















   ..
       !! processed by numpydoc !!

.. py:property:: val
   :type: float


   
   Get or set the Define the value of the history variable
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'HISTORY_NODE_SET'






