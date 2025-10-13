





:class:`IcfdDatabaseNodout`
===========================


.. py:class:: icfd_database_nodout.IcfdDatabaseNodout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_NODOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseNodout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~outlv`
            - Get or set the Determines if the output file should be dumped.
          * - :py:attr:`~dtout`
            - Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
          * - :py:attr:`~nid1`
            - Get or set the Node IDs.
          * - :py:attr:`~nid2`
            - Get or set the Node IDs.
          * - :py:attr:`~nid3`
            - Get or set the Node IDs.
          * - :py:attr:`~nid4`
            - Get or set the Node IDs.
          * - :py:attr:`~nid5`
            - Get or set the Node IDs.
          * - :py:attr:`~nid6`
            - Get or set the Node IDs.
          * - :py:attr:`~nid7`
            - Get or set the Node IDs.
          * - :py:attr:`~nid8`
            - Get or set the Node IDs.


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

    from icfd_database_nodout import IcfdDatabaseNodout

Property detail
---------------

.. py:property:: outlv
   :type: int


   
   Get or set the Determines if the output file should be dumped.
   EQ.0: No output file is generated.
   EQ.1: The output file is generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid1
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid3
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid4
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid5
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid6
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid7
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid8
   :type: Optional[int]


   
   Get or set the Node IDs.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_NODOUT'






