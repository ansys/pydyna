





:class:`SetBox`
===============


.. py:class:: set_box.SetBox(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_BOX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetBox

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID of new beam set. All beam sets should have a unique set ID.
          * - :py:attr:`~box`
            - dynamic array of box set ids..
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

    from set_box import SetBox

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID of new beam set. All beam sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: box
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of box set ids..
















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
   :value: 'BOX'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





