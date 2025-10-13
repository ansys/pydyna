





:class:`InterfaceLinkingNodeSetLocal`
=====================================


.. py:class:: interface_linking_node_set_local.InterfaceLinkingNodeSetLocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_LINKING_NODE_SET_LOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceLinkingNodeSetLocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Node set ID to be moved by interface file, see *SET_NODE.
          * - :py:attr:`~ifid`
            - Get or set the Interface ID in interface file.
          * - :py:attr:`~fx`
            - Get or set the The ID of a *DEFINE_FUNCTION which determines the x direction displacement scale factor. See Remarks.
          * - :py:attr:`~fy`
            - Get or set the The ID of a *DEFINE_FUNCTION which determines the y direction displacement scale factor. See Remarks.
          * - :py:attr:`~fz`
            - Get or set the The ID of a *DEFINE_FUNCTION which determines the z direction displacement scale factor. See Remarks.
          * - :py:attr:`~lcid`
            - Get or set the Local coordinate system ID for transforming displacements.
          * - :py:attr:`~lnid`
            - Get or set the Local node ID for transforming displacements.
          * - :py:attr:`~usec`
            - Get or set the Flag to indicate the use of the coordinate system in the linking file
          * - :py:attr:`~usen`
            - Get or set the Flag to indicate the use of the node displacement in the linking


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

    from interface_linking_node_set_local import InterfaceLinkingNodeSetLocal

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID to be moved by interface file, see *SET_NODE.
















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

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID for transforming displacements.
















   ..
       !! processed by numpydoc !!

.. py:property:: lnid
   :type: Optional[int]


   
   Get or set the Local node ID for transforming displacements.
















   ..
       !! processed by numpydoc !!

.. py:property:: usec
   :type: int


   
   Get or set the Flag to indicate the use of the coordinate system in the linking file
   during displacement transformation. See Remarks.
   EQ.0: Linking file coordinate system is ignored.
   EQ.1: Linking file coordinate system is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: usen
   :type: int


   
   Get or set the Flag to indicate the use of the node displacement in the linking
   file during displacement transformation. See Remarks.
   EQ.0: Node displacement is not used.
   EQ.1: Node displacement is used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'LINKING_NODE_SET_LOCAL'






