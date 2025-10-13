





:class:`ControlCoarsen`
=======================


.. py:class:: control_coarsen.ControlCoarsen(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_COARSEN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlCoarsen

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~icoarse`
            - Get or set the Coarsening flag:
          * - :py:attr:`~angle`
            - Get or set the Allowable angle change between neighboring elements. Adjacent elements which are flat to within ANGLE degrees are merged.
          * - :py:attr:`~nseed`
            - Get or set the Number of seed nodes (optional).
          * - :py:attr:`~psid`
            - Get or set the Part set ID. all the parts defined in this set will be prevented from been coarsened.
          * - :py:attr:`~smax`
            - Get or set the Max element size. For ICOARSE=2 no elements larger then this size will be created.
          * - :py:attr:`~n1`
            - Get or set the Optional list of seed node IDs for extra searching.
          * - :py:attr:`~n2`
            - Get or set the Optional list of seed node IDs for extra searching.
          * - :py:attr:`~n3`
            - Get or set the Optional list of seed node IDs for extra searching.
          * - :py:attr:`~n4`
            - Get or set the Optional list of seed node IDs for extra searching.
          * - :py:attr:`~n5`
            - Get or set the Optional list of seed node IDs for extra searching.
          * - :py:attr:`~n6`
            - Get or set the Optional list of seed node IDs for extra searching.
          * - :py:attr:`~n7`
            - Get or set the Optional list of seed node IDs for extra searching.
          * - :py:attr:`~n8`
            - Get or set the Optional list of seed node IDs for extra searching.


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

    from control_coarsen import ControlCoarsen

Property detail
---------------

.. py:property:: icoarse
   :type: int


   
   Get or set the Coarsening flag:
   EQ.0: Do not coarsen (default),
   EQ.1: Coarsen mesh at beginning of simulation.
   EQ.2: Coarsen mesh at beginning of simulation for forming model
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: Optional[float]


   
   Get or set the Allowable angle change between neighboring elements. Adjacent elements which are flat to within ANGLE degrees are merged.
   Suggested starting value = 8.0 degrees.
















   ..
       !! processed by numpydoc !!

.. py:property:: nseed
   :type: int


   
   Get or set the Number of seed nodes (optional).
   EQ.0: use only automatic searching (default).
   EQ.n: also search starting with node IDs given below (maximum = 8 nodes).
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID. all the parts defined in this set will be prevented from been coarsened.
















   ..
       !! processed by numpydoc !!

.. py:property:: smax
   :type: Optional[float]


   
   Get or set the Max element size. For ICOARSE=2 no elements larger then this size will be created.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!

.. py:property:: n7
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!

.. py:property:: n8
   :type: int


   
   Get or set the Optional list of seed node IDs for extra searching.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'COARSEN'






