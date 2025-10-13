





:class:`NodeScalarValue`
========================


.. py:class:: node_scalar_value.NodeScalarValue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA NODE_SCALAR_VALUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: NodeScalarValue

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Scalar node ID.
          * - :py:attr:`~x1`
            - Get or set the Initial value of Ith degree of freedom .
          * - :py:attr:`~x2`
            - Get or set the Initial value of Ith degree of freedom
          * - :py:attr:`~x3`
            - Get or set the Initial value of Ith degree of freedom
          * - :py:attr:`~ndof`
            - Get or set the Number of degrees-of-freedom.


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

    from node_scalar_value import NodeScalarValue

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Scalar node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: float


   
   Get or set the Initial value of Ith degree of freedom .
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: float


   
   Get or set the Initial value of Ith degree of freedom
















   ..
       !! processed by numpydoc !!

.. py:property:: x3
   :type: float


   
   Get or set the Initial value of Ith degree of freedom
















   ..
       !! processed by numpydoc !!

.. py:property:: ndof
   :type: int


   
   Get or set the Number of degrees-of-freedom.
   EQ.0: fully constrained.
   EQ.1: one degree-of-freedom.
   EQ.2: two degree-of-freedom.
   EQ.3: three degree-of-freedom.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'NODE'


.. py:attribute:: subkeyword
   :value: 'SCALAR_VALUE'






