





:class:`ElementShellSourceSink`
===============================


.. py:class:: element_shell_source_sink.ElementShellSourceSink(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SHELL_SOURCE_SINK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementShellSourceSink

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nssr`
            - Get or set the Node set at source. Provide an ordered set of nodes between corner nodes, which include the corner nodes
          * - :py:attr:`~nssk`
            - Get or set the Node set at sink. Provide an ordered set of nodes between corner nodes, which include the corner nodes
          * - :py:attr:`~pid`
            - Get or set the Part ID of work piece.


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

    from element_shell_source_sink import ElementShellSourceSink

Property detail
---------------

.. py:property:: nssr
   :type: Optional[int]


   
   Get or set the Node set at source. Provide an ordered set of nodes between corner nodes, which include the corner nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: nssk
   :type: Optional[int]


   
   Get or set the Node set at sink. Provide an ordered set of nodes between corner nodes, which include the corner nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of work piece.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SHELL_SOURCE_SINK'






