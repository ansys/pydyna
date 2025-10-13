





:class:`DefineCurveBoxAdaptivity`
=================================


.. py:class:: define_curve_box_adaptivity.DefineCurveBoxAdaptivity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_BOX_ADAPTIVITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveBoxAdaptivity

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Curve ID; must be unique. The curve must be closed: its first and
          * - :py:attr:`~pid`
            - Get or set the Sheet blank Part ID, as in *PART
          * - :py:attr:`~level`
            - Get or set the Adaptive refinement levels, similar to the field MAXLVL in
          * - :py:attr:`~dist1`
            - Get or set the Depth in the ..-direction that the curve defined with Card 2 will
          * - :py:attr:`~x`
            - Get or set the X coordinate of a point on the curve.
          * - :py:attr:`~y`
            - Get or set the Y coordinate of a point on the curve.
          * - :py:attr:`~z`
            - Get or set the Z coordinate of a point on the curve.
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

    from define_curve_box_adaptivity import DefineCurveBoxAdaptivity

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Curve ID; must be unique. The curve must be closed: its first and
   last point must coincide. See Examples
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Sheet blank Part ID, as in *PART
















   ..
       !! processed by numpydoc !!

.. py:property:: level
   :type: Optional[int]


   
   Get or set the Adaptive refinement levels, similar to the field MAXLVL in
   *CONTROL_ADAPTIVE. See Remark
















   ..
       !! processed by numpydoc !!

.. py:property:: dist1
   :type: Optional[float]


   
   Get or set the Depth in the ..-direction that the curve defined with Card 2 will
   be extruded. Currently this variable must be input as a negative value.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the X coordinate of a point on the curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Y coordinate of a point on the curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Z coordinate of a point on the curve.
















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
   :value: 'CURVE_BOX_ADAPTIVITY'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





