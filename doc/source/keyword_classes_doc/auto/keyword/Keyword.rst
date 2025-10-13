





:class:`Keyword`
================


.. py:class:: keyword.Keyword(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA KEYWORD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Keyword

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~memory`
            - Get or set the Memory size in units of words to be allocated.
          * - :py:attr:`~memory2`
            - Get or set the For MPP, defines the memory allocation in words for each of the cores except the first core.
          * - :py:attr:`~ncpu`
            - Get or set the Number of CPUs "n" to be used during the analysis


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

    from keyword import Keyword

Property detail
---------------

.. py:property:: memory
   :type: Optional[str]


   
   Get or set the Memory size in units of words to be allocated.
















   ..
       !! processed by numpydoc !!

.. py:property:: memory2
   :type: Optional[str]


   
   Get or set the For MPP, defines the memory allocation in words for each of the cores except the first core.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncpu
   :type: Optional[int]


   
   Get or set the Number of CPUs "n" to be used during the analysis
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'KEYWORD'


.. py:attribute:: subkeyword
   :value: 'KEYWORD'






