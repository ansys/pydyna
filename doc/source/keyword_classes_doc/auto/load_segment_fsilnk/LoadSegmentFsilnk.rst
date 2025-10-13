





:class:`LoadSegmentFsilnk`
==========================


.. py:class:: load_segment_fsilnk.LoadSegmentFsilnk(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEGMENT_FSILNK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSegmentFsilnk

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the Filename of the interface linking file
          * - :py:attr:`~nint`
            - Get or set the Number of couplings for which the previous run provides pressure data
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID (see *DEFINE_CURVE) or function ID (see *DE-FINE_FUNCTION). The curve referred to by LCID provides a scale
          * - :py:attr:`~id1`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
          * - :py:attr:`~id2`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
          * - :py:attr:`~id3`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
          * - :py:attr:`~id4`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
          * - :py:attr:`~id5`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
          * - :py:attr:`~id6`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
          * - :py:attr:`~id7`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
          * - :py:attr:`~id8`
            - Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs


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

    from load_segment_fsilnk import LoadSegmentFsilnk

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Filename of the interface linking file
















   ..
       !! processed by numpydoc !!

.. py:property:: nint
   :type: Optional[int]


   
   Get or set the Number of couplings for which the previous run provides pressure data
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID (see *DEFINE_CURVE) or function ID (see *DE-FINE_FUNCTION). The curve referred to by LCID provides a scale
   factor as a function of time. The pressure data that is read in from
   the fsilnk file is scaled according to this value
















   ..
       !! processed by numpydoc !!

.. py:property:: id1
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!

.. py:property:: id2
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!

.. py:property:: id3
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!

.. py:property:: id4
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!

.. py:property:: id5
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!

.. py:property:: id6
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!

.. py:property:: id7
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!

.. py:property:: id8
   :type: Optional[int]


   
   Get or set the These must match COUPIDs from the *CONSTRAINED_LA-       GRANGE_IN_SOLID card from the previous runs. These IDs
   specify which of the first run's couplings are propagated into this
   run through pressure data read from the fsilnk file.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_FSILNK'






