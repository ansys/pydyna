





:class:`BoundarySpcNode`
========================


.. py:class:: boundary_spc_node.BoundarySpcNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SPC_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySpcNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nodes`
            - Get the table of nodes.
          * - :py:attr:`~id`
            - Get or set the ID keyword option
          * - :py:attr:`~heading`
            - Get or set the Descriptor. We suggest using unique descriptions.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from boundary_spc_node import BoundarySpcNode

Property detail
---------------

.. py:property:: nodes
   :type: pandas.DataFrame


   
   Get the table of nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID keyword option
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the Descriptor. We suggest using unique descriptions.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SPC_NODE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





