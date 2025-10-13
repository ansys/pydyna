





:class:`DefineCurveSmooth`
==========================


.. py:class:: define_curve_smooth.DefineCurveSmooth(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_SMOOTH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveSmooth

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID. A unique number must be defined.
          * - :py:attr:`~sidr`
            - Get or set the Stress initialization by dynamic relaxation:
          * - :py:attr:`~dist`
            - Get or set the Total distance tool will travel (area under curve).
          * - :py:attr:`~tstart`
            - Get or set the Time curve starts to rise.
          * - :py:attr:`~tend`
            - Get or set the Time curve returns to zero.
          * - :py:attr:`~trise`
            - Get or set the Rise time.
          * - :py:attr:`~vmax`
            - Get or set the Maximum velocity (maximum value of curve).
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

    from define_curve_smooth import DefineCurveSmooth

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID. A unique number must be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidr
   :type: int


   
   Get or set the Stress initialization by dynamic relaxation:
   EQ.0: Load curve used in transient analysis only or for other applications (default),
   EQ.1: Load curve used in stress initialization but not transient analysis,
   EQ.2: Load curve applies to both initialization and transient analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: dist
   :type: Optional[float]


   
   Get or set the Total distance tool will travel (area under curve).
















   ..
       !! processed by numpydoc !!

.. py:property:: tstart
   :type: Optional[float]


   
   Get or set the Time curve starts to rise.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: Optional[float]


   
   Get or set the Time curve returns to zero.
   If TEND is nonzero, VMAX will be computed automatically to satisfy required travel distance DIST. Input either TEND or VMAX.
















   ..
       !! processed by numpydoc !!

.. py:property:: trise
   :type: Optional[float]


   
   Get or set the Rise time.
















   ..
       !! processed by numpydoc !!

.. py:property:: vmax
   :type: Optional[float]


   
   Get or set the Maximum velocity (maximum value of curve).
   If VMAX is nonzero, TEND will be computed automatically to satisfy required travel distance DIST. Input either TEND or VMAX.
















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
   :value: 'CURVE_SMOOTH'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





