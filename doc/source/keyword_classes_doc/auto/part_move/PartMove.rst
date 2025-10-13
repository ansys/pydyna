





:class:`PartMove`
=================


.. py:class:: part_move.PartMove(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_MOVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartMove

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part id or Part Set id
          * - :py:attr:`~xmov`
            - Get or set the Move shell part set, in the x-direction by the incremental distance, XMOV.
          * - :py:attr:`~ymov`
            - Get or set the Move shell part set, in the y-direction by the incremental distance, YMOV.
          * - :py:attr:`~zmov`
            - Get or set the Move shell part set, in the z-direction by the incremental distance, ZMOV.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID to define incremental displacement in local coordinate system.  All displacements, XMOV, YMOV, and ZMOV, are with respect to CID.
          * - :py:attr:`~ifset`
            - Get or set the part move flag: 0: part id; 1: part set id


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

    from part_move import PartMove

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part id or Part Set id
















   ..
       !! processed by numpydoc !!

.. py:property:: xmov
   :type: Optional[float]


   
   Get or set the Move shell part set, in the x-direction by the incremental distance, XMOV.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymov
   :type: Optional[float]


   
   Get or set the Move shell part set, in the y-direction by the incremental distance, YMOV.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmov
   :type: Optional[float]


   
   Get or set the Move shell part set, in the z-direction by the incremental distance, ZMOV.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID to define incremental displacement in local coordinate system.  All displacements, XMOV, YMOV, and ZMOV, are with respect to CID.
   EQ.0: global
















   ..
       !! processed by numpydoc !!

.. py:property:: ifset
   :type: int


   
   Get or set the part move flag: 0: part id; 1: part set id
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'MOVE'






