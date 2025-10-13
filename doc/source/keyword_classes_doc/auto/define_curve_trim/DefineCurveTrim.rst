





:class:`DefineCurveTrim`
========================


.. py:class:: define_curve_trim.DefineCurveTrim(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_TRIM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveTrim

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tcid`
            - Get or set the ID number for trim curve. A unique number has to be defined.
          * - :py:attr:`~tctype`
            - Get or set the Trim curve type:
          * - :py:attr:`~tctol`
            - Get or set the Tolerance limiting size of small elements created during trimming (default = 0.25)
          * - :py:attr:`~cx`
            - Get or set the x-coordinate of trim curve Defined if and only if TCTYPE=1.
          * - :py:attr:`~cy`
            - Get or set the y-coordinate of trim curve Defined if and only if TCTYPE=1.
          * - :py:attr:`~filename`
            - Get or set the Name of IGES database containing trim curve(s). Defined if and only if TCTYPE=2.
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

    from define_curve_trim import DefineCurveTrim

Property detail
---------------

.. py:property:: tcid
   :type: Optional[int]


   
   Get or set the ID number for trim curve. A unique number has to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: tctype
   :type: int


   
   Get or set the Trim curve type:
   EQ.1: digitized curve provided,
   EQ.2: IGES trim curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: tctol
   :type: float


   
   Get or set the Tolerance limiting size of small elements created during trimming (default = 0.25)
















   ..
       !! processed by numpydoc !!

.. py:property:: cx
   :type: float


   
   Get or set the x-coordinate of trim curve Defined if and only if TCTYPE=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cy
   :type: float


   
   Get or set the y-coordinate of trim curve Defined if and only if TCTYPE=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of IGES database containing trim curve(s). Defined if and only if TCTYPE=2.
















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
   :value: 'CURVE_TRIM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





