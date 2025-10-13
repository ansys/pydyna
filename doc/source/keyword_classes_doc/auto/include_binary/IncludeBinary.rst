





:class:`IncludeBinary`
======================


.. py:class:: include_binary.IncludeBinary(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_BINARY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeBinary

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the File name of file to be included in this keyword file.


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

    from include_binary import IncludeBinary

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the File name of file to be included in this keyword file.
   Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'BINARY'






