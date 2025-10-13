





:class:`SetNodeColumn`
======================


.. py:class:: set_node_column.SetNodeColumn(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_NODE_COLUMN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetNodeColumn

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
          * - :py:attr:`~nid`
            - Get or set the Nodal ID.
          * - :py:attr:`~a1`
            - Get or set the First nodal attribute default value is 0.0.
          * - :py:attr:`~a2`
            - Get or set the Second nodal attribute default value is 0.0.
          * - :py:attr:`~a3`
            - Get or set the Third nodal attribute default value is 0.0.
          * - :py:attr:`~a4`
            - Get or set the Fourth nodal attribute default value is 0.0.
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

    from set_node_column import SetNodeColumn

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

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Nodal ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: float


   
   Get or set the First nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: float


   
   Get or set the Second nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: float


   
   Get or set the Third nodal attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: a4
   :type: float


   
   Get or set the Fourth nodal attribute default value is 0.0.
















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
   :value: 'NODE_COLUMN'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





