





:class:`IncludeMultiscale`
==========================


.. py:class:: include_multiscale.IncludeMultiscale(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_MULTISCALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeMultiscale

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for this multiscale local model.  This ID is used in the keyword *DEFINE_‌MULTISCALE.  Any unique integer will do
          * - :py:attr:`~filename`
            - Get or set the Name of file from which to read the local model definition.


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

    from include_multiscale import IncludeMultiscale

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for this multiscale local model.  This ID is used in the keyword *DEFINE_‌MULTISCALE.  Any unique integer will do
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of file from which to read the local model definition.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'MULTISCALE'






