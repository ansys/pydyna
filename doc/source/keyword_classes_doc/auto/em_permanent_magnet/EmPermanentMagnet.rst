





:class:`EmPermanentMagnet`
==========================


.. py:class:: em_permanent_magnet.EmPermanentMagnet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_PERMANENT_MAGNET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmPermanentMagnet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the MID
          * - :py:attr:`~partid`
            - Get or set the PART ID
          * - :py:attr:`~mtype`
            - Get or set the Magnet definition type :
          * - :py:attr:`~north`
            - Get or set the Set of nodes/segments  of the north face of magnet
          * - :py:attr:`~south`
            - Get or set the Set of nodes/segments  of the south face of magnet
          * - :py:attr:`~hc`
            - Get or set the Coercive force. If a negative value is entered, it will give the value as a function of time
          * - :py:attr:`~x_nid1`
            - Get or set the Orientation of magnetization vector if MTYPE=3.
          * - :py:attr:`~y_nid2`
            - Get or set the Orientation of magnetization vector if MTYPE=3.
          * - :py:attr:`~z`
            - Get or set the Orientation of magnetization vector if MTYPE=3.


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

    from em_permanent_magnet import EmPermanentMagnet

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the MID
















   ..
       !! processed by numpydoc !!

.. py:property:: partid
   :type: Optional[int]


   
   Get or set the PART ID
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Magnet definition type :
   EQ.0 : Magnet defined by two node sets for Northand South Poles.
   EQ.1 : Magnet defined by two segments sets for Northand South Poles.
   EQ.3 : Magnet defined by a global vector orientation.
   EQ.4 : Magnet defined by a global vector orientation given by two node IDs.
















   ..
       !! processed by numpydoc !!

.. py:property:: north
   :type: Optional[int]


   
   Get or set the Set of nodes/segments  of the north face of magnet
















   ..
       !! processed by numpydoc !!

.. py:property:: south
   :type: Optional[int]


   
   Get or set the Set of nodes/segments  of the south face of magnet
















   ..
       !! processed by numpydoc !!

.. py:property:: hc
   :type: Optional[float]


   
   Get or set the Coercive force. If a negative value is entered, it will give the value as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: x_nid1
   :type: Optional[float]


   
   Get or set the Orientation of magnetization vector if MTYPE=3.
   Two node IDs defining the magnetization vector if MTYPE=4.
















   ..
       !! processed by numpydoc !!

.. py:property:: y_nid2
   :type: Optional[float]


   
   Get or set the Orientation of magnetization vector if MTYPE=3.
   Two node IDs defining the magnetization vector if MTYPE=4.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Orientation of magnetization vector if MTYPE=3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'PERMANENT_MAGNET'






