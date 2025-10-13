





:class:`IgaTiedEdgeToEdge`
==========================


.. py:class:: iga_tied_edge_to_edge.IgaTiedEdgeToEdge(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA IGA_TIED_EDGE_TO_EDGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IgaTiedEdgeToEdge

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Apply coupling to entities referenced by the ID field along topologically connected edges. The next field, TYPE, specifies the type of entity to which ID refers because entities of different kinds, such as parts and part sets, are not uniquely numbered.  Currently (as of June 2020), Currently, no types requiring an ID are supported. This field is reserved for future enhancements
          * - :py:attr:`~type`
            - Get or set the Type of ID:
          * - :py:attr:`~form`
            - Get or set the Coupling formulation:
          * - :py:attr:`~sfd`
            - Get or set the Scaling factor for displacement penalty stiffness
          * - :py:attr:`~sfr`
            - Get or set the Scaling factor for rotational penalty stiffness
          * - :py:attr:`~sft`
            - Get or set the Scaling factor for thin constraint penalty stiffness (rotation free elements)


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

    from iga_tied_edge_to_edge import IgaTiedEdgeToEdge

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Apply coupling to entities referenced by the ID field along topologically connected edges. The next field, TYPE, specifies the type of entity to which ID refers because entities of different kinds, such as parts and part sets, are not uniquely numbered.  Currently (as of June 2020), Currently, no types requiring an ID are supported. This field is reserved for future enhancements
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Type of ID:
   EQ.0:   Include all topological connections in the model.No ID required
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Coupling formulation:
   EQ.0:   Penalty - based tied contact
















   ..
       !! processed by numpydoc !!

.. py:property:: sfd
   :type: float


   
   Get or set the Scaling factor for displacement penalty stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: sfr
   :type: float


   
   Get or set the Scaling factor for rotational penalty stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: sft
   :type: float


   
   Get or set the Scaling factor for thin constraint penalty stiffness (rotation free elements)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'IGA'


.. py:attribute:: subkeyword
   :value: 'TIED_EDGE_TO_EDGE'






