





:class:`DefineCurveFunction`
============================


.. py:class:: define_curve_function.DefineCurveFunction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_FUNCTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveFunction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load Curve ID.
          * - :py:attr:`~sidr`
            - Get or set the Stress initialization by dynamic relaxation:
          * - :py:attr:`~sfa`
            - Get or set the Scale factor for abscissa value.  This is useful for simple modifications.
          * - :py:attr:`~sfo`
            - Get or set the Scale factor for ordinate value (function).  This is useful for simple modifications.
          * - :py:attr:`~offa`
            - Get or set the Offset for abscissa values, see explanation below.
          * - :py:attr:`~offo`
            - Get or set the Offset for ordinate values (function), see explanation below.
          * - :py:attr:`~dattyp`
            - Get or set the Data type. Usually 0, set to 1 only for general xy data. This affects how offsets are applied. General xy data curves refer to curves whose abcissa values do not increase monotonically. Generally, DATTYP=0 for time dependent curves, force versus displacement curves, and stress strain curves.
          * - :py:attr:`~function`
            - Get or set the Arithmetic expression involving a combination of the following possibilities.
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

    from define_curve_function import DefineCurveFunction

Property detail
---------------

.. py:property:: lcid
   :type: int


   
   Get or set the Load Curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidr
   :type: int


   
   Get or set the Stress initialization by dynamic relaxation:
   EQ.0:  load curve used in transient analysis only or for other applications,
   EQ.1:  load curve used in stress initialization but not transient analysis,
   EQ.2:  load curve applies to both initialization and transient analysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfa
   :type: float


   
   Get or set the Scale factor for abscissa value.  This is useful for simple modifications.
   EQ.0.0:  default set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfo
   :type: float


   
   Get or set the Scale factor for ordinate value (function).  This is useful for simple modifications.
   EQ.0.0:  default set to 1.0..
















   ..
       !! processed by numpydoc !!

.. py:property:: offa
   :type: float


   
   Get or set the Offset for abscissa values, see explanation below.
















   ..
       !! processed by numpydoc !!

.. py:property:: offo
   :type: float


   
   Get or set the Offset for ordinate values (function), see explanation below.
















   ..
       !! processed by numpydoc !!

.. py:property:: dattyp
   :type: int


   
   Get or set the Data type. Usually 0, set to 1 only for general xy data. This affects how offsets are applied. General xy data curves refer to curves whose abcissa values do not increase monotonically. Generally, DATTYP=0 for time dependent curves, force versus displacement curves, and stress strain curves.
















   ..
       !! processed by numpydoc !!

.. py:property:: function
   :type: Optional[str]


   
   Get or set the Arithmetic expression involving a combination of the following possibilities.
















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
   :value: 'CURVE_FUNCTION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





