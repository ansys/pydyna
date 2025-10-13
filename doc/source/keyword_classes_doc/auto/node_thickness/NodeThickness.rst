





:class:`NodeThickness`
======================


.. py:class:: node_thickness.NodeThickness(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA NODE_THICKNESS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: NodeThickness

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id1`
            - Get or set the Node ID. If GENERATE option is active, ID1 serves as the starting node.
          * - :py:attr:`~thk`
            - Get or set the Thickness at node ID1 (ignored if GENERATE option is active).
          * - :py:attr:`~id2`
            - Get or set the Ending node if GENERATE option is active.
          * - :py:attr:`~inc`
            - Get or set the Increment in node numbers if GENERATE option is active.


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

    from node_thickness import NodeThickness

Property detail
---------------

.. py:property:: id1
   :type: Optional[int]


   
   Get or set the Node ID. If GENERATE option is active, ID1 serves as the starting node.
















   ..
       !! processed by numpydoc !!

.. py:property:: thk
   :type: Optional[float]


   
   Get or set the Thickness at node ID1 (ignored if GENERATE option is active).
















   ..
       !! processed by numpydoc !!

.. py:property:: id2
   :type: Optional[int]


   
   Get or set the Ending node if GENERATE option is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: inc
   :type: Optional[int]


   
   Get or set the Increment in node numbers if GENERATE option is active.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'NODE'


.. py:attribute:: subkeyword
   :value: 'THICKNESS'






