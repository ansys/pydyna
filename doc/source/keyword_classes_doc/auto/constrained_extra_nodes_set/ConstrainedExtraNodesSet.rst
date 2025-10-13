





:class:`ConstrainedExtraNodesSet`
=================================


.. py:class:: constrained_extra_nodes_set.ConstrainedExtraNodesSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_EXTRA_NODES_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedExtraNodesSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of rigid body to which the nodes will be added, see *PART.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID of added nodes, see *SET_NODE.
          * - :py:attr:`~iflag`
            - Get or set the This flag is meaningful if and only if the inertia properties of the Part ID


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

    from constrained_extra_nodes_set import ConstrainedExtraNodesSet

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of rigid body to which the nodes will be added, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID of added nodes, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: int


   
   Get or set the This flag is meaningful if and only if the inertia properties of the Part ID
   are defined in PART_INERTIA. If set to unity, the center-of-gravity, the
   translational mass, and the inertia matrix of the PID will be updated to reflect the
   merged nodal masses of the node or node set. If IFLAG is defaulted to zero, the merged nodes will not affect the properties defined in
   PART_INERTIA since it is assumed the properties already account for merged nodes.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'EXTRA_NODES_SET'






