





:class:`LoadBlastSegmentSet`
============================


.. py:class:: load_blast_segment_set.LoadBlastSegmentSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_BLAST_SEGMENT_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadBlastSegmentSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bid`
            - Get or set the Blast source ID (see *LOAD_BLAST_ENHANCED).
          * - :py:attr:`~ssid`
            - Get or set the Segment set ID (see *SET_SEGMENT).
          * - :py:attr:`~alepid`
            - Get or set the Part ID of ALE ambient part underlying this segment to be loaded by this blast (see *PART and *SECTION_SOLID, AET=5).This applies only when the blast load is coupled to an ALE air domain.
          * - :py:attr:`~sfnrb`
            - Get or set the Scale factor for the ambient element non-reflecting boundary condition.  Shocks waves reflected back to the ambient elements can be attenuated with this feature.  A value of 1.0 works well for most situations.
          * - :py:attr:`~scalep`
            - Get or set the Pressure scale factor.


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

    from load_blast_segment_set import LoadBlastSegmentSet

Property detail
---------------

.. py:property:: bid
   :type: Optional[int]


   
   Get or set the Blast source ID (see *LOAD_BLAST_ENHANCED).
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID (see *SET_SEGMENT).
















   ..
       !! processed by numpydoc !!

.. py:property:: alepid
   :type: Optional[int]


   
   Get or set the Part ID of ALE ambient part underlying this segment to be loaded by this blast (see *PART and *SECTION_SOLID, AET=5).This applies only when the blast load is coupled to an ALE air domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfnrb
   :type: float


   
   Get or set the Scale factor for the ambient element non-reflecting boundary condition.  Shocks waves reflected back to the ambient elements can be attenuated with this feature.  A value of 1.0 works well for most situations.
















   ..
       !! processed by numpydoc !!

.. py:property:: scalep
   :type: float


   
   Get or set the Pressure scale factor.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'BLAST_SEGMENT_SET'






