





:class:`NodeNodes`
==================


.. py:class:: node_nodes.NodeNodes(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA NODE_NODES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: NodeNodes

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~x`
            - Get or set the x-coordinate.
          * - :py:attr:`~y`
            - Get or set the y-coordinate.
          * - :py:attr:`~z`
            - Get or set the z-coordinate.
          * - :py:attr:`~tc`
            - Get or set the Translational constraint:
          * - :py:attr:`~rc`
            - Get or set the Rotational constraint:


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

    from node_nodes import NodeNodes

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: int


   
   Get or set the Translational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x displacement,
   EQ.2: constrained y displacement,
   EQ.3: constrained z displacement,
   EQ.4: constrained x and y displacements,
   EQ.5: constrained y and z displacements,
   EQ.6: constrained z and x displacements,
   EQ.7: constrained x, y, and z displacements.
















   ..
       !! processed by numpydoc !!

.. py:property:: rc
   :type: int


   
   Get or set the Rotational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x rotation,
   EQ.2: constrained y rotation,
   EQ.3: constrained z rotation,
   EQ.4: constrained x and y rotations,
   EQ.5: constrained y and z rotations,
   EQ.6: constrained z and x rotations,
   EQ.7: constrained x, y, and z rotations.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'NODE'


.. py:attribute:: subkeyword
   :value: 'NODES'






