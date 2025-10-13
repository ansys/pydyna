





:class:`SetNodeListGenerateIncrementCollect`
============================================


.. py:class:: set_node_list_generate_increment_collect.SetNodeListGenerateIncrementCollect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_NODE_LIST_GENERATE_INCREMENT_COLLECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetNodeListGenerateIncrementCollect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Node set ID. All node sets should have a unique set ID.
          * - :py:attr:`~da1`
            - Get or set the First nodal attribute default value is 0.0.
          * - :py:attr:`~da2`
            - Get or set the Second nodal attribute default value is 0.0.
          * - :py:attr:`~da3`
            - Get or set the Third nodal attribute default value is 0.0.
          * - :py:attr:`~da4`
            - Get or set the Fourth nodal attribute default value is 0.0.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~its`
            - Get or set the Specify coupling type across different scales in two-scale co-simulation. This flag should only be included for node sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
          * - :py:attr:`~bbeg`
            - Get or set the First node ID in the first block.
          * - :py:attr:`~bend`
            - Get or set the Last node ID in the first block.
          * - :py:attr:`~incr`
            - Get or set the Node ID increment. Node IDs BBEG, BBEG+INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
          * - :py:attr:`~title`
            - Get or set the Additional title line


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

    from set_node_list_generate_increment_collect import SetNodeListGenerateIncrementCollect

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Node set ID. All node sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: its
   :type: str


   
   Get or set the Specify coupling type across different scales in two-scale co-simulation. This flag should only be included for node sets that provide coupling information in the input file referred to by *INCLUDE_COSIM;
   EQ.1:   Tied contact coupling
   EQ.2 : Solid - in - shell immersed coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: bbeg
   :type: Optional[int]


   
   Get or set the First node ID in the first block.
















   ..
       !! processed by numpydoc !!

.. py:property:: bend
   :type: Optional[int]


   
   Get or set the Last node ID in the first block.
















   ..
       !! processed by numpydoc !!

.. py:property:: incr
   :type: Optional[int]


   
   Get or set the Node ID increment. Node IDs BBEG, BBEG+INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'NODE_LIST_GENERATE_INCREMENT_COLLECT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





