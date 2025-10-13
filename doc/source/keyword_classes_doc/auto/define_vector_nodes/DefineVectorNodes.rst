





:class:`DefineVectorNodes`
==========================


.. py:class:: define_vector_nodes.DefineVectorNodes(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_VECTOR_NODES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineVectorNodes

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vid`
            - Get or set the Vector ID. A unique number has to be used.
          * - :py:attr:`~nodet`
            - Get or set the Nodal point to define tail of vector.
          * - :py:attr:`~nodeh`
            - Get or set the Nodal point to define head of vector.
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

    from define_vector_nodes import DefineVectorNodes

Property detail
---------------

.. py:property:: vid
   :type: int


   
   Get or set the Vector ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodet
   :type: int


   
   Get or set the Nodal point to define tail of vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: nodeh
   :type: int


   
   Get or set the Nodal point to define head of vector.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'VECTOR_NODES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





