





:class:`SetBeamGenerateIncrementCollect`
========================================


.. py:class:: set_beam_generate_increment_collect.SetBeamGenerateIncrementCollect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_BEAM_GENERATE_INCREMENT_COLLECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetBeamGenerateIncrementCollect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Beam element set ID.
          * - :py:attr:`~bbeg`
            - Get or set the First beam element ID in block.
          * - :py:attr:`~bend`
            - Get or set the Last beam element ID in block.
          * - :py:attr:`~incr`
            - Get or set the Beam ID increment. Beam IDs BBEG, BBEG + INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
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

    from set_beam_generate_increment_collect import SetBeamGenerateIncrementCollect

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Beam element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: bbeg
   :type: Optional[int]


   
   Get or set the First beam element ID in block.
















   ..
       !! processed by numpydoc !!

.. py:property:: bend
   :type: Optional[int]


   
   Get or set the Last beam element ID in block.
















   ..
       !! processed by numpydoc !!

.. py:property:: incr
   :type: Optional[int]


   
   Get or set the Beam ID increment. Beam IDs BBEG, BBEG + INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
















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
   :value: 'BEAM_GENERATE_INCREMENT_COLLECT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





