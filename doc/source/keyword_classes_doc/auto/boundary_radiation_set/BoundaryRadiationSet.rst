





:class:`BoundaryRadiationSet`
=============================


.. py:class:: boundary_radiation_set.BoundaryRadiationSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_RADIATION_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryRadiationSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see also *SET_SEGMENT.
          * - :py:attr:`~type`
            - Get or set the Radiation type:
          * - :py:attr:`~pserod`
            - Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode
          * - :py:attr:`~rflcid`
            - Get or set the Load curve ID for radiation factor f, see *DEFINE_CURVE.
          * - :py:attr:`~rfmult`
            - Get or set the Curve multiplier for f, see *DEFINE_CURVE.
          * - :py:attr:`~tilcid`
            - Get or set the Load curve ID for T-infinity versus time, see *DEFINE_CURVE.
          * - :py:attr:`~timult`
            - Get or set the Curve multiplier for T-infinity.
          * - :py:attr:`~loc`
            - Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:


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

    from boundary_radiation_set import BoundaryRadiationSet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see also *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Radiation type:
   EQ.1: radiation boundary to environment
















   ..
       !! processed by numpydoc !!

.. py:property:: pserod
   :type: Optional[int]


   
   Get or set the Part set ID for updating boundary segments exposed to the environment as solid elements erode
















   ..
       !! processed by numpydoc !!

.. py:property:: rflcid
   :type: int


   
   Get or set the Load curve ID for radiation factor f, see *DEFINE_CURVE.
   GT.0: function versus time,
   EQ.0: use constant multiplier value, RFMULT (default),
   LT.0: function versus temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: rfmult
   :type: float


   
   Get or set the Curve multiplier for f, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: tilcid
   :type: int


   
   Get or set the Load curve ID for T-infinity versus time, see *DEFINE_CURVE.
   EQ.0: use constant multiplier, TIMULT (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: timult
   :type: float


   
   Get or set the Curve multiplier for T-infinity.
















   ..
       !! processed by numpydoc !!

.. py:property:: loc
   :type: int


   
   Get or set the Application of surface for thermal shell elements, see paramter, TSHELL, in the *CONTROL_SHELL input:
   EQ.-1: lower surface of thermal shell element,
   EQ. 1: upper surface of thermal shell element
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'RADIATION_SET'






