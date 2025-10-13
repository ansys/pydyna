





:class:`InterfaceSsiAuxNode`
============================


.. py:class:: interface_ssi_aux_node.InterfaceSsiAuxNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_SSI_AUX_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceSsiAuxNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~gmset`
            - Get or set the Identifier for this set of recorded motions to be referred to in *INTERFACE_SSI. Must be unique.
          * - :py:attr:`~setid`
            - Get or set the Segment set or node set ID where motions are to be recorded.


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

    from interface_ssi_aux_node import InterfaceSsiAuxNode

Property detail
---------------

.. py:property:: gmset
   :type: Optional[int]


   
   Get or set the Identifier for this set of recorded motions to be referred to in *INTERFACE_SSI. Must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Segment set or node set ID where motions are to be recorded.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'SSI_AUX_NODE'






