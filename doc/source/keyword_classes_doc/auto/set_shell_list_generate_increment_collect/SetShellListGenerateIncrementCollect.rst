





:class:`SetShellListGenerateIncrementCollect`
=============================================


.. py:class:: set_shell_list_generate_increment_collect.SetShellListGenerateIncrementCollect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SHELL_LIST_GENERATE_INCREMENT_COLLECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetShellListGenerateIncrementCollect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Shell element set ID. All shell sets should have a unique set ID.
          * - :py:attr:`~da1`
            - Get or set the First attribute default value is 0.0.
          * - :py:attr:`~da2`
            - Get or set the Second attribute default value is 0.0.
          * - :py:attr:`~da3`
            - Get or set the Third attribute default value is 0.0.
          * - :py:attr:`~da4`
            - Get or set the Fourth attribute default value is 0.0.
          * - :py:attr:`~bbeg`
            - Get or set the First shell element ID in block.
          * - :py:attr:`~bend`
            - Get or set the Last shell element ID in block.
          * - :py:attr:`~incr`
            - Get or set the Shell element ID increment. Shell element IDs BBEG, BBEG + INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
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

    from set_shell_list_generate_increment_collect import SetShellListGenerateIncrementCollect

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Shell element set ID. All shell sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: bbeg
   :type: Optional[int]


   
   Get or set the First shell element ID in block.
















   ..
       !! processed by numpydoc !!

.. py:property:: bend
   :type: Optional[int]


   
   Get or set the Last shell element ID in block.
















   ..
       !! processed by numpydoc !!

.. py:property:: incr
   :type: Optional[int]


   
   Get or set the Shell element ID increment. Shell element IDs BBEG, BBEG + INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
















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
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'SHELL_LIST_GENERATE_INCREMENT_COLLECT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





