





:class:`NodeScalar`
===================


.. py:class:: node_scalar.NodeScalar(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA NODE_SCALAR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: NodeScalar

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nid`
            - Get or set the Scalar node ID.
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

    from node_scalar import NodeScalar

Property detail
---------------

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Scalar node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndof
   :type: int


   
   Get or set the Number of degrees-of-freedom.
   EQ.0: fully constrained.
   EQ.1: one degree-of-freedom.
   EQ.2: two degrees-of-freedom.
   EQ.3: three degrees-of-freedom.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'NODE'


.. py:attribute:: subkeyword
   :value: 'SCALAR'






