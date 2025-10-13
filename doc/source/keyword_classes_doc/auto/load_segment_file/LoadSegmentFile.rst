





:class:`LoadSegmentFile`
========================


.. py:class:: load_segment_file.LoadSegmentFile(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEGMENT_FILE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSegmentFile

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the Filename of the interface linking file
          * - :py:attr:`~lcid`
            - Get or set the Optional Load curve ID defining segment pressure scale factor versus time.


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

    from load_segment_file import LoadSegmentFile

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Filename of the interface linking file
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Optional Load curve ID defining segment pressure scale factor versus time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_FILE'






