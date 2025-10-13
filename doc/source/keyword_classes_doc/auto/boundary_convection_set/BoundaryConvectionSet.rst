





:class:`BoundaryConvectionSet`
==============================


.. py:class:: boundary_convection_set.BoundaryConvectionSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_CONVECTION_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryConvectionSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see *SET_SEGMENT.
          * - :py:attr:`~pserod`
            - Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode; see Remark 4.
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

    from boundary_convection_set import BoundaryConvectionSet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: pserod
   :type: Optional[int]


   
   Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode; see Remark 4.
















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
   :value: 'CONVECTION_SET'






