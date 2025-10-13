





:class:`DatabaseFormat`
=======================


.. py:class:: database_format.DatabaseFormat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_FORMAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseFormat

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~iform`
            - Get or set the Output format for D3PLOT and D3THDT files
          * - :py:attr:`~ibinary`
            - Get or set the Word size of the binary output files (D3PLOT , D3THDT, D3DRLF) and interface files for 64-bit computers such as CRAY and NEC.


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

    from database_format import DatabaseFormat

Property detail
---------------

.. py:property:: iform
   :type: int


   
   Get or set the Output format for D3PLOT and D3THDT files
   EQ.0: LS-DYNA database format (default),
   EQ.1: ANSYS database format,
   EQ.2: Both LS-DYNA and ANSYS database formats.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibinary
   :type: int


   
   Get or set the Word size of the binary output files (D3PLOT , D3THDT, D3DRLF) and interface files for 64-bit computers such as CRAY and NEC.
   EQ.0: default 64-bit format,
   EQ.1: 32-bit IEEE format.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'FORMAT'






