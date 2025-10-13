





:class:`DefineCurveTriaxialLimitFromFld`
========================================


.. py:class:: define_curve_triaxial_limit_from_fld.DefineCurveTriaxialLimitFromFld(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_TRIAXIAL_LIMIT_FROM_FLD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveTriaxialLimitFromFld

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the FLD Load curve ID to be created.
          * - :py:attr:`~a1`
            - Get or set the Abscissa values. Only pairs have to be defined.
          * - :py:attr:`~o1`
            - Get or set the Ordinate (function) values. Only pairs have to be defined.
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

    from define_curve_triaxial_limit_from_fld import DefineCurveTriaxialLimitFromFld

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the FLD Load curve ID to be created.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: float


   
   Get or set the Abscissa values. Only pairs have to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: o1
   :type: float


   
   Get or set the Ordinate (function) values. Only pairs have to be defined.
















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
   :value: 'CURVE_TRIAXIAL_LIMIT_FROM_FLD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





