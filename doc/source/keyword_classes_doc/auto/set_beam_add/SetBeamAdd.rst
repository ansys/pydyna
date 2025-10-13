





:class:`SetBeamAdd`
===================


.. py:class:: set_beam_add.SetBeamAdd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_BEAM_ADD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetBeamAdd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID of new beam set. All beam sets should have a unique set ID.
          * - :py:attr:`~beams`
            - dynamic array of beam set ids..
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

    from set_beam_add import SetBeamAdd

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID of new beam set. All beam sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: beams
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of beam set ids..
















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
   :value: 'BEAM_ADD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





