





:class:`DefineFilter`
=====================


.. py:class:: define_filter.DefineFilter(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FILTER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFilter

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID of a GEOMETRY defining high explosive particle domain.
          * - :py:attr:`~title`
            - Get or set the Geometry type
          * - :py:attr:`~type`
            - Get or set the One of the 3 currently defined filter types: DISCRETE, CONTINUOUS, or CHAIN.
          * - :py:attr:`~data1`
            - Get or set the Filter type specific data, which determines what the filter does.
          * - :py:attr:`~data2`
            - Get or set the Filter type specific data, which determines what the filter does.
          * - :py:attr:`~data3`
            - Get or set the Filter type specific data, which determines what the filter does.
          * - :py:attr:`~data4`
            - Get or set the Filter type specific data, which determines what the filter does.
          * - :py:attr:`~data5`
            - Get or set the Filter type specific data, which determines what the filter does.
          * - :py:attr:`~data6`
            - Get or set the Filter type specific data, which determines what the filter does.
          * - :py:attr:`~data7`
            - Get or set the Filter type specific data, which determines what the filter does.


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

    from define_filter import DefineFilter

Property detail
---------------

.. py:property:: id
   :type: int


   
   Get or set the ID of a GEOMETRY defining high explosive particle domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Geometry type
   EQ.1: box
   EQ.2: sphere
   EQ.3: cylinder
   EQ.4: ellipsoid
   EQ.5: hemisphere (see Remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: Optional[str]


   
   Get or set the One of the 3 currently defined filter types: DISCRETE, CONTINUOUS, or CHAIN.
















   ..
       !! processed by numpydoc !!

.. py:property:: data1
   :type: Optional[str]


   
   Get or set the Filter type specific data, which determines what the filter does.
















   ..
       !! processed by numpydoc !!

.. py:property:: data2
   :type: Optional[str]


   
   Get or set the Filter type specific data, which determines what the filter does.
















   ..
       !! processed by numpydoc !!

.. py:property:: data3
   :type: Optional[str]


   
   Get or set the Filter type specific data, which determines what the filter does.
















   ..
       !! processed by numpydoc !!

.. py:property:: data4
   :type: Optional[str]


   
   Get or set the Filter type specific data, which determines what the filter does.
















   ..
       !! processed by numpydoc !!

.. py:property:: data5
   :type: Optional[str]


   
   Get or set the Filter type specific data, which determines what the filter does.
















   ..
       !! processed by numpydoc !!

.. py:property:: data6
   :type: Optional[str]


   
   Get or set the Filter type specific data, which determines what the filter does.
















   ..
       !! processed by numpydoc !!

.. py:property:: data7
   :type: Optional[str]


   
   Get or set the Filter type specific data, which determines what the filter does.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'FILTER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





