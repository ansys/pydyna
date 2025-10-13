





:class:`DefineTable2D`
======================


.. py:class:: define_table_2d.DefineTable2D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_TABLE_2D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineTable2D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tbid`
            - Get or set the Table ID. Tables and Load curves may not share common ID's.  LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably.
          * - :py:attr:`~sfa`
            - Get or set the Scale factor for value.
          * - :py:attr:`~offa`
            - Get or set the Offset for values.
          * - :py:attr:`~value`
            - Get or set the Load curve will be defined corresponding to this value, e.g., this value could be a strain rate, see purpose above.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID used by this value.
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

    from define_table_2d import DefineTable2D

Property detail
---------------

.. py:property:: tbid
   :type: Optional[int]


   
   Get or set the Table ID. Tables and Load curves may not share common ID's.  LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfa
   :type: float


   
   Get or set the Scale factor for value.
















   ..
       !! processed by numpydoc !!

.. py:property:: offa
   :type: float


   
   Get or set the Offset for values.
















   ..
       !! processed by numpydoc !!

.. py:property:: value
   :type: float


   
   Get or set the Load curve will be defined corresponding to this value, e.g., this value could be a strain rate, see purpose above.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID used by this value.
















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
   :value: 'TABLE_2D'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





