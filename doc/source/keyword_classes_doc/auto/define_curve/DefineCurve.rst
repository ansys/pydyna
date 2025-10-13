





:class:`DefineCurve`
====================


.. py:class:: define_curve.DefineCurve(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurve

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
          * - :py:attr:`~sidr`
            - Get or set the Stress initialization by dynamic relaxation:
          * - :py:attr:`~sfa`
            - Get or set the Scale factor for abcissa value. This is useful for simple modifications.
          * - :py:attr:`~sfo`
            - Get or set the Scale factor for ordinate value (function). This is useful for simple modifications.
          * - :py:attr:`~offa`
            - Get or set the Offset for abcissa values.
          * - :py:attr:`~offo`
            - Get or set the Offset for ordinate values (function).
          * - :py:attr:`~dattyp`
            - Get or set the Data type.This affects how offsets are applied.
          * - :py:attr:`~lcint`
            - Get or set the The number of discretization intervals to use for this curve. If 0 is input, the value of LCINT from *CONTROL_SOLUTION will be used.
          * - :py:attr:`~curves`
            - Get the table of curves.
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

    from define_curve import DefineCurve

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidr
   :type: int


   
   Get or set the Stress initialization by dynamic relaxation:
   EQ.0: load curve used in transient analysis only or for other applications,
   EQ.1: load curve used in stress initialization but not transient analysis,
   EQ.2: load curve applies to both initialization and transient analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfa
   :type: float


   
   Get or set the Scale factor for abcissa value. This is useful for simple modifications.
   EQ.0.0: default set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfo
   :type: float


   
   Get or set the Scale factor for ordinate value (function). This is useful for simple modifications.
   EQ.0.0: default set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: offa
   :type: float


   
   Get or set the Offset for abcissa values.
















   ..
       !! processed by numpydoc !!

.. py:property:: offo
   :type: float


   
   Get or set the Offset for ordinate values (function).
















   ..
       !! processed by numpydoc !!

.. py:property:: dattyp
   :type: int


   
   Get or set the Data type.This affects how offsets are applied.
   EQ.-2:for fabric stress vs. strain curves(*MAT_FABRIC)as described below.Thickness flag for norminal stress calculation.
   EQ.0:general case for time dependent curves,force versus displacement curves and stress strain curves.
   EQ.1:for general (x,y) data curves whose abscissa values do not increase monotonically.
   EQ.6:for general (r,s) data(coordinates in a 2D parametric space) whose values do not increase momotonically.Use for definition of trimming polygons for trimmed NURBS(*ELEMENT_SHELL_NURBS_PATCH,NL.GT.0).
   EQ.-100:        for defining the proxy, α, from experiments for the chemical shrinkage coefficient as a function of temperature (see *MAT_ADD_CHEM_SHRINKAGE for details)
















   ..
       !! processed by numpydoc !!

.. py:property:: lcint
   :type: int


   
   Get or set the The number of discretization intervals to use for this curve. If 0 is input, the value of LCINT from *CONTROL_SOLUTION will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: curves
   :type: pandas.DataFrame


   
   Get the table of curves.
















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
   :value: 'CURVE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





