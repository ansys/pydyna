





:class:`DatabaseBinaryD3Prop`
=============================


.. py:class:: database_binary_d3prop.DatabaseBinaryD3Prop(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_BINARY_D3PROP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseBinaryD3Prop

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ifile`
            - Get or set the Specify file for D3PROP output.  (This can also be defined on the command line by adding d3prop = 1 or d3prop = 2 which also sets IMATL =  IWALL = 1)
          * - :py:attr:`~imatl`
            - Get or set the Output *EOS, *HOURGLASS, *MAT, *PART and *SECTION data.
          * - :py:attr:`~iwall`
            - Get or set the Output *RIGIDWALL data.


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

    from database_binary_d3prop import DatabaseBinaryD3Prop

Property detail
---------------

.. py:property:: ifile
   :type: int


   
   Get or set the Specify file for D3PROP output.  (This can also be defined on the command line by adding d3prop = 1 or d3prop = 2 which also sets IMATL =  IWALL = 1)
   EQ.1: Output data at the end of the first d3plot file.
   EQ.2: Output data to the file d3prop.
















   ..
       !! processed by numpydoc !!

.. py:property:: imatl
   :type: int


   
   Get or set the Output *EOS, *HOURGLASS, *MAT, *PART and *SECTION data.
   EQ.0: No
   EQ.1: Yes
















   ..
       !! processed by numpydoc !!

.. py:property:: iwall
   :type: int


   
   Get or set the Output *RIGIDWALL data.
   EQ.0: No
   EQ.1: Yes
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'BINARY_D3PROP'






