





:class:`SetSegmentAdd`
======================


.. py:class:: set_segment_add.SetSegmentAdd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SEGMENT_ADD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetSegmentAdd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Segment set ID. All segment sets should have a unique set ID.
          * - :py:attr:`~sets`
            - dynamic array of set segment ids..
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

    from set_segment_add import SetSegmentAdd

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Segment set ID. All segment sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sets
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of set segment ids..
















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
   :value: 'SEGMENT_ADD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





