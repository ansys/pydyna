





:class:`DefineCurveDuplicate`
=============================


.. py:class:: define_curve_duplicate.DefineCurveDuplicate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_DUPLICATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveDuplicate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
          * - :py:attr:`~rlcid`
            - Get or set the Reference load curve ID
          * - :py:attr:`~sfa`
            - Get or set the Scale factor for abcissa value. This is useful for simple modifications.
          * - :py:attr:`~sfo`
            - Get or set the Scale factor for ordinate value (function). This is useful for simple modifications.
          * - :py:attr:`~offa`
            - Get or set the Offset for abcissa values.
          * - :py:attr:`~offo`
            - Get or set the Offset for ordinate values (function).
          * - :py:attr:`~dattyp`
            - Get or set the Data type. Usually 0, set to 1 only for general xy data. This affects how offsets are applied. General xy data curves refer to curves whose abcissa values do not increase monotonically. Generally, DATTYP=0 for time dependent curves, force versus displacement curves, and stress strain curves.
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

    from define_curve_duplicate import DefineCurveDuplicate

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
















   ..
       !! processed by numpydoc !!

.. py:property:: rlcid
   :type: Optional[int]


   
   Get or set the Reference load curve ID
















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


   
   Get or set the Data type. Usually 0, set to 1 only for general xy data. This affects how offsets are applied. General xy data curves refer to curves whose abcissa values do not increase monotonically. Generally, DATTYP=0 for time dependent curves, force versus displacement curves, and stress strain curves.
















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
   :value: 'CURVE_DUPLICATE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





