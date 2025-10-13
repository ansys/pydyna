





:class:`InterfaceLinkingNodeNode`
=================================


.. py:class:: interface_linking_node_node.InterfaceLinkingNodeNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_LINKING_NODE_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceLinkingNodeNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID to be moved by interface file, see *NODE.
          * - :py:attr:`~ifid`
            - Get or set the Interface ID in interface file.
          * - :py:attr:`~fx`
            - Get or set the The ID of a *DEFINE_FUNCTION which determines the x direction displacement scale factor. See Remarks.
          * - :py:attr:`~fy`
            - Get or set the The ID of a *DEFINE_FUNCTION which determines the y direction displacement scale factor. See Remarks.
          * - :py:attr:`~fz`
            - Get or set the The ID of a *DEFINE_FUNCTION which determines the z direction displacement scale factor. See Remarks.


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

    from interface_linking_node_node import InterfaceLinkingNodeNode

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID to be moved by interface file, see *NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifid
   :type: Optional[int]


   
   Get or set the Interface ID in interface file.
















   ..
       !! processed by numpydoc !!

.. py:property:: fx
   :type: Optional[int]


   
   Get or set the The ID of a *DEFINE_FUNCTION which determines the x direction displacement scale factor. See Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: fy
   :type: Optional[int]


   
   Get or set the The ID of a *DEFINE_FUNCTION which determines the y direction displacement scale factor. See Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: fz
   :type: Optional[int]


   
   Get or set the The ID of a *DEFINE_FUNCTION which determines the z direction displacement scale factor. See Remarks.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'LINKING_NODE_NODE'






