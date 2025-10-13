





:class:`BoundaryNonReflecting2D`
================================


.. py:class:: boundary_non_reflecting_2d.BoundaryNonReflecting2D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_NON_REFLECTING_2D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryNonReflecting2D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Node set ID, see *SET_NODE.
          * - :py:attr:`~ad`
            - Get or set the Default activation flag for dilatational waves.
          * - :py:attr:`~as_`
            - Get or set the Default activation flag for shear waves.


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

    from boundary_non_reflecting_2d import BoundaryNonReflecting2D

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID, see *SET_NODE.
   LT.0.0:|NSID| is the id of *SET_SEGMENT
















   ..
       !! processed by numpydoc !!

.. py:property:: ad
   :type: int


   
   Get or set the Default activation flag for dilatational waves.
   EQ.0: on (default),
   NE.0: off.
















   ..
       !! processed by numpydoc !!

.. py:property:: as_
   :type: int


   
   Get or set the Default activation flag for shear waves.
   EQ.0: on (default),
   NE.0: off.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'NON_REFLECTING_2D'






