





:class:`ControlFormingStoning`
==============================


.. py:class:: control_forming_stoning.ControlFormingStoning(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_STONING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingStoning

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~istone`
            - Get or set the activate this capability.
          * - :py:attr:`~length`
            - Get or set the the length of the stone.
          * - :py:attr:`~width`
            - Get or set the Width of the stone
          * - :py:attr:`~step`
            - Get or set the The step size for the stone to move in each step
          * - :py:attr:`~direction`
            - Get or set the Number of automatically determined stoning direction(s).
          * - :py:attr:`~reverse`
            - Get or set the Surface normal reversing option.
          * - :py:attr:`~method`
            - Get or set the Stoning method.
          * - :py:attr:`~node1`
            - Get or set the Node1 and Node2 define the orientation of the stone
          * - :py:attr:`~node2`
            - Get or set the Node1 and Node2 define the orientation of the stone.
          * - :py:attr:`~setid`
            - Get or set the id, itype 1: node set, itype 2: shell set
          * - :py:attr:`~itype`
            - Get or set the 1:node set, 2 shell set


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

    from control_forming_stoning import ControlFormingStoning

Property detail
---------------

.. py:property:: istone
   :type: Optional[int]


   
   Get or set the activate this capability.
















   ..
       !! processed by numpydoc !!

.. py:property:: length
   :type: Optional[float]


   
   Get or set the the length of the stone.
















   ..
       !! processed by numpydoc !!

.. py:property:: width
   :type: Optional[int]


   
   Get or set the Width of the stone
















   ..
       !! processed by numpydoc !!

.. py:property:: step
   :type: float


   
   Get or set the The step size for the stone to move in each step
















   ..
       !! processed by numpydoc !!

.. py:property:: direction
   :type: Optional[int]


   
   Get or set the Number of automatically determined stoning direction(s).
















   ..
       !! processed by numpydoc !!

.. py:property:: reverse
   :type: int


   
   Get or set the Surface normal reversing option.
   EQ.0: do not reverse surface normals.
   EQ.1: reverse surface normals
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: int


   
   Get or set the Stoning method.
   EQ.0: curvature-based method.
















   ..
       !! processed by numpydoc !!

.. py:property:: node1
   :type: Optional[int]


   
   Get or set the Node1 and Node2 define the orientation of the stone
















   ..
       !! processed by numpydoc !!

.. py:property:: node2
   :type: Optional[int]


   
   Get or set the Node1 and Node2 define the orientation of the stone.
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the id, itype 1: node set, itype 2: shell set
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the 1:node set, 2 shell set
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_STONING'






