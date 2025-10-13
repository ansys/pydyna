





:class:`ElementSeatbeltRetractor`
=================================


.. py:class:: element_seatbelt_retractor.ElementSeatbeltRetractor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SEATBELT_RETRACTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementSeatbeltRetractor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sbrid`
            - Get or set the Retractor ID. A unique number has to be used.
          * - :py:attr:`~sbrnid`
            - Get or set the Retractor node ID
          * - :py:attr:`~sbid`
            - Get or set the Seat belt element ID
          * - :py:attr:`~sid1`
            - Get or set the Sensor ID 1
          * - :py:attr:`~sid2`
            - Get or set the Sensor ID 2
          * - :py:attr:`~sid3`
            - Get or set the Sensor ID 3
          * - :py:attr:`~sid4`
            - Get or set the Sensor ID 4
          * - :py:attr:`~tdel`
            - Get or set the Time delay after sensor triggers.
          * - :py:attr:`~pull`
            - Get or set the Amount of pull-out between time delay ending and retractor locking, a length value.
          * - :py:attr:`~llcid`
            - Get or set the Load curve for loading (Pull-out, Force).
          * - :py:attr:`~ulcid`
            - Get or set the Load curve for unloading (Pull-out, Force).
          * - :py:attr:`~lfed`
            - Get or set the Fed length.


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

    from element_seatbelt_retractor import ElementSeatbeltRetractor

Property detail
---------------

.. py:property:: sbrid
   :type: Optional[int]


   
   Get or set the Retractor ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbrnid
   :type: Optional[int]


   
   Get or set the Retractor node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: sbid
   :type: Optional[int]


   
   Get or set the Seat belt element ID
















   ..
       !! processed by numpydoc !!

.. py:property:: sid1
   :type: int


   
   Get or set the Sensor ID 1
















   ..
       !! processed by numpydoc !!

.. py:property:: sid2
   :type: int


   
   Get or set the Sensor ID 2
















   ..
       !! processed by numpydoc !!

.. py:property:: sid3
   :type: int


   
   Get or set the Sensor ID 3
















   ..
       !! processed by numpydoc !!

.. py:property:: sid4
   :type: int


   
   Get or set the Sensor ID 4
















   ..
       !! processed by numpydoc !!

.. py:property:: tdel
   :type: float


   
   Get or set the Time delay after sensor triggers.
















   ..
       !! processed by numpydoc !!

.. py:property:: pull
   :type: float


   
   Get or set the Amount of pull-out between time delay ending and retractor locking, a length value.
















   ..
       !! processed by numpydoc !!

.. py:property:: llcid
   :type: int


   
   Get or set the Load curve for loading (Pull-out, Force).
















   ..
       !! processed by numpydoc !!

.. py:property:: ulcid
   :type: int


   
   Get or set the Load curve for unloading (Pull-out, Force).
















   ..
       !! processed by numpydoc !!

.. py:property:: lfed
   :type: float


   
   Get or set the Fed length.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SEATBELT_RETRACTOR'






