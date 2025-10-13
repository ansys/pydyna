





:class:`InterfaceComponentNode`
===============================


.. py:class:: interface_component_node.InterfaceComponentNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_COMPONENT_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceComponentNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for this interface in the linking file.
          * - :py:attr:`~title`
            - Get or set the Title for this interface.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID, see *SET_NODE(for NODE and SPH options) .
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID.
          * - :py:attr:`~nid`
            - Get or set the Node ID.


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

    from interface_component_node import InterfaceComponentNode

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for this interface in the linking file.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Title for this interface.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID, see *SET_NODE(for NODE and SPH options) .
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'COMPONENT_NODE'






