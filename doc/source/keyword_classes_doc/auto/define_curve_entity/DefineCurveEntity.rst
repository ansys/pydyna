





:class:`DefineCurveEntity`
==========================


.. py:class:: define_curve_entity.DefineCurveEntity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_ENTITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveEntity

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
          * - :py:attr:`~sfa`
            - Get or set the Scale factor for axis value. This is useful for simple modifications.    EQ.0.0: default set to 1.0.
          * - :py:attr:`~sfo`
            - Get or set the Scale factor for radius values. This is useful for simple modifications. EQ.0.0: default set to 1.0.
          * - :py:attr:`~sfr`
            - Get or set the Scale factor for circular radius. This is useful for simple      modifications. EQ.0.0: default set to 1.0.
          * - :py:attr:`~offa`
            - Get or set the Offset for axis values, see explanation below.
          * - :py:attr:`~offo`
            - Get or set the Offset for radius values, see explanation below.
          * - :py:attr:`~offr`
            - Get or set the Offset for circular radius, see explanation below.
          * - :py:attr:`~ai`
            - Get or set the Z-axis coordinates along the axis of rotation.
          * - :py:attr:`~oi`
            - Get or set the Radial coordinates from the axis of rotation
          * - :py:attr:`~ri`
            - Get or set the Radius of arc between points (Ai,Oi) and (Ai+1,Oi+1). If zero, a straight line segment is assumed.
          * - :py:attr:`~iflag`
            - Get or set the Defined if |Ri| > 0. Set to 1 if center of arc is inside axisymmetric surface and to -1 if the center is outside the axisymmetric surface.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_curve_entity import DefineCurveEntity

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfa
   :type: float


   
   Get or set the Scale factor for axis value. This is useful for simple modifications.    EQ.0.0: default set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfo
   :type: float


   
   Get or set the Scale factor for radius values. This is useful for simple modifications. EQ.0.0: default set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfr
   :type: float


   
   Get or set the Scale factor for circular radius. This is useful for simple      modifications. EQ.0.0: default set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: offa
   :type: float


   
   Get or set the Offset for axis values, see explanation below.
















   ..
       !! processed by numpydoc !!

.. py:property:: offo
   :type: float


   
   Get or set the Offset for radius values, see explanation below.
















   ..
       !! processed by numpydoc !!

.. py:property:: offr
   :type: float


   
   Get or set the Offset for circular radius, see explanation below.
















   ..
       !! processed by numpydoc !!

.. py:property:: ai
   :type: float


   
   Get or set the Z-axis coordinates along the axis of rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: oi
   :type: float


   
   Get or set the Radial coordinates from the axis of rotation
















   ..
       !! processed by numpydoc !!

.. py:property:: ri
   :type: float


   
   Get or set the Radius of arc between points (Ai,Oi) and (Ai+1,Oi+1). If zero, a straight line segment is assumed.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: int


   
   Get or set the Defined if |Ri| > 0. Set to 1 if center of arc is inside axisymmetric surface and to -1 if the center is outside the axisymmetric surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CURVE_ENTITY'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





