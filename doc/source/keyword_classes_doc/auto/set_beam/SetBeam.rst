





:class:`SetBeam`
================


.. py:class:: set_beam.SetBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Beam element set ID.
          * - :py:attr:`~element`
            - dynamic array of beam element ids..
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

    from set_beam import SetBeam

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Beam element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: element
   :type: ansys.dyna.core.lib.series_card.SeriesCard


   
   dynamic array of beam element ids..
















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
   :value: 'BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





