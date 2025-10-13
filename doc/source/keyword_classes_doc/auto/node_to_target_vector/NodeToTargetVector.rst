





:class:`NodeToTargetVector`
===========================


.. py:class:: node_to_target_vector.NodeToTargetVector(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA NODE_TO_TARGET_VECTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: NodeToTargetVector

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID on a part best fitted to the target.
          * - :py:attr:`~xdelta`
            - Get or set the Difference in X-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
          * - :py:attr:`~ydelta`
            - Get or set the Difference in Y-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
          * - :py:attr:`~zdelta`
            - Get or set the Difference in Z-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.


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

    from node_to_target_vector import NodeToTargetVector

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID on a part best fitted to the target.
















   ..
       !! processed by numpydoc !!

.. py:property:: xdelta
   :type: float


   
   Get or set the Difference in X-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
















   ..
       !! processed by numpydoc !!

.. py:property:: ydelta
   :type: float


   
   Get or set the Difference in Y-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
















   ..
       !! processed by numpydoc !!

.. py:property:: zdelta
   :type: float


   
   Get or set the Difference in Z-coordinates of the normal distance from the target (typically scan data) to a node of a part best fitted to the target.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'NODE'


.. py:attribute:: subkeyword
   :value: 'TO_TARGET_VECTOR'






