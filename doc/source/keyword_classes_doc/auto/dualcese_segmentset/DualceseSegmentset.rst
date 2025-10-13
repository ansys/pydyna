





:class:`DualceseSegmentset`
===========================


.. py:class:: dualcese_segmentset.DualceseSegmentset(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_SEGMENTSET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseSegmentset

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set ID.  All segment sets should have a unique set ID
          * - :py:attr:`~n1`
            - Get or set the Nodal point N1
          * - :py:attr:`~n2`
            - Get or set the Nodal point N2
          * - :py:attr:`~n3`
            - Get or set the Nodal point N3.To define a line segement, set N3=N2
          * - :py:attr:`~n4`
            - Get or set the Nodal point N4. To define a triangular segment N4=N3. To Define a line segement, set N4 = N2


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

    from dualcese_segmentset import DualceseSegmentset

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID.  All segment sets should have a unique set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Nodal point N1
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Nodal point N2
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Nodal point N3.To define a line segement, set N3=N2
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Nodal point N4. To define a triangular segment N4=N3. To Define a line segement, set N4 = N2
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'SEGMENTSET'






