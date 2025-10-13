





:class:`BoundaryConvectionSegment`
==================================


.. py:class:: boundary_convection_segment.BoundaryConvectionSegment(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_CONVECTION_SEGMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryConvectionSegment

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the First node ID defining the segment.
          * - :py:attr:`~n2`
            - Get or set the Second node ID defining the segment.
          * - :py:attr:`~n3`
            - Get or set the Third Node ID's defining segment.
          * - :py:attr:`~n4`
            - Get or set the Fourth node ID defining the segment.
          * - :py:attr:`~hlcid`
            - Get or set the Load curve ID for heat transfer coefficient, h:
          * - :py:attr:`~hmult`
            - Get or set the Curve multiplier for h.
          * - :py:attr:`~tlcid`
            - Get or set the Load curve ID for T-infinity versus time, see *DEFINE_CURVE:
          * - :py:attr:`~tmult`
            - Get or set the Curve multiplier for T-infinity.
          * - :py:attr:`~loc`
            - Get or set the LOC Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input::


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

    from boundary_convection_segment import BoundaryConvectionSegment

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the First node ID defining the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Second node ID defining the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Third Node ID's defining segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Fourth node ID defining the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: hlcid
   :type: Optional[int]


   
   Get or set the Load curve ID for heat transfer coefficient, h:
   GT.0: function versus time,
   EQ.0: use constant multiplier value, HMULT,
   LT.0: function versus temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmult
   :type: float


   
   Get or set the Curve multiplier for h.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlcid
   :type: Optional[int]


   
   Get or set the Load curve ID for T-infinity versus time, see *DEFINE_CURVE:
   EQ.0: use constant multiplier value, TMULT.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmult
   :type: float


   
   Get or set the Curve multiplier for T-infinity.
















   ..
       !! processed by numpydoc !!

.. py:property:: loc
   :type: int


   
   Get or set the LOC Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input::
   EQ.-1: lower surface of thermal shell element,
   EQ. 1: upper surface of thermal shell element.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'CONVECTION_SEGMENT'






