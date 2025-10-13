





:class:`DefineTable`
====================


.. py:class:: define_table.DefineTable(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_TABLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineTable

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
          * - :py:attr:`~points`
            - Interpolation points for subsequent DEFINE_CURVE keywords..
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

    from define_table import DefineTable

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

.. py:property:: points
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   Interpolation points for subsequent DEFINE_CURVE keywords..
















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
   :value: 'TABLE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





