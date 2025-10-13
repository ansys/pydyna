





:class:`BoundaryNonReflecting`
==============================


.. py:class:: boundary_non_reflecting.BoundaryNonReflecting(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_NON_REFLECTING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryNonReflecting

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see *SET_SEGMENT.
          * - :py:attr:`~ad`
            - Get or set the Default activation flag for dilatational waves.
          * - :py:attr:`~as_`
            - Get or set the Default activation flag for shear waves.


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

    from boundary_non_reflecting import BoundaryNonReflecting

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: ad
   :type: float


   
   Get or set the Default activation flag for dilatational waves.
   EQ.0.0: on (default),
   NE.0.0: off.
















   ..
       !! processed by numpydoc !!

.. py:property:: as_
   :type: float


   
   Get or set the Default activation flag for shear waves.
   EQ.0.0: on (default),
   NE.0.0: off.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'NON_REFLECTING'






