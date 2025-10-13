





:class:`DefineTableMatrix`
==========================


.. py:class:: define_table_matrix.DefineTableMatrix(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_TABLE_MATRIX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineTableMatrix

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tbid`
            - Get or set the Table ID. Tables and Load curves may not share common ID's.  LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A negative value of TBID switches the interpretation of rows and columns in the read matrix.
          * - :py:attr:`~filename`
            - Get or set the Name of file containing table data (stored as a matrix).
          * - :py:attr:`~nrow`
            - Get or set the Number of rows in the matrix, same as number of rows in the file FILENAME
          * - :py:attr:`~ncol`
            - Get or set the Number of columns in the matrix, same as number of data entries per row in the file FILENAME
          * - :py:attr:`~srow`
            - Get or set the Scale factor for row data
          * - :py:attr:`~scol`
            - Get or set the Scale factor for column data
          * - :py:attr:`~sval`
            - Get or set the Scale factor for matrix values
          * - :py:attr:`~orow`
            - Get or set the Offset for row data
          * - :py:attr:`~ocol`
            - Get or set the Offset for column data
          * - :py:attr:`~oval`
            - Get or set the Offset for matrix values
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

    from define_table_matrix import DefineTableMatrix

Property detail
---------------

.. py:property:: tbid
   :type: Optional[int]


   
   Get or set the Table ID. Tables and Load curves may not share common ID's.  LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A negative value of TBID switches the interpretation of rows and columns in the read matrix.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of file containing table data (stored as a matrix).
















   ..
       !! processed by numpydoc !!

.. py:property:: nrow
   :type: Optional[int]


   
   Get or set the Number of rows in the matrix, same as number of rows in the file FILENAME
















   ..
       !! processed by numpydoc !!

.. py:property:: ncol
   :type: Optional[int]


   
   Get or set the Number of columns in the matrix, same as number of data entries per row in the file FILENAME
















   ..
       !! processed by numpydoc !!

.. py:property:: srow
   :type: float


   
   Get or set the Scale factor for row data
















   ..
       !! processed by numpydoc !!

.. py:property:: scol
   :type: float


   
   Get or set the Scale factor for column data
















   ..
       !! processed by numpydoc !!

.. py:property:: sval
   :type: float


   
   Get or set the Scale factor for matrix values
















   ..
       !! processed by numpydoc !!

.. py:property:: orow
   :type: Optional[float]


   
   Get or set the Offset for row data
















   ..
       !! processed by numpydoc !!

.. py:property:: ocol
   :type: Optional[float]


   
   Get or set the Offset for column data
















   ..
       !! processed by numpydoc !!

.. py:property:: oval
   :type: Optional[float]


   
   Get or set the Offset for matrix values
















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
   :value: 'TABLE_MATRIX'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





