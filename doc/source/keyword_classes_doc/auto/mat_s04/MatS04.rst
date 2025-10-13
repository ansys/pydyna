





:class:`MatS04`
===============


.. py:class:: mat_s04.MatS04(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_S04 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatS04

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material number. A unique number has to be used.
          * - :py:attr:`~lcd`
            - Get or set the Load curve ID (see *DEFINE_CURVE) describing force as a function of displacement or moment as a function of rotation relationship. The load curve must define the response in the negative and positive quadrants and pass through point Image.  Negative data point(s) must come first in the curve definition, where negative values represent compression in the case of a translational spring.
          * - :py:attr:`~lcr`
            - Get or set the Optional load curve describing scale factor on force or moment as a function of relative velocity or rotational velocity. The load curve most define the response in the negative and positive quadrants and pass through point (0,0).
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

    from mat_s04 import MatS04

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material number. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd
   :type: Optional[int]


   
   Get or set the Load curve ID (see *DEFINE_CURVE) describing force as a function of displacement or moment as a function of rotation relationship. The load curve must define the response in the negative and positive quadrants and pass through point Image.  Negative data point(s) must come first in the curve definition, where negative values represent compression in the case of a translational spring.
   LCD may also be a table ID (see *DEFINE_TABLE). The table gives for each loading rate a load curve ID defining the force-displacement (or moment-rotation) curve. Values between the data points are computed by linear interpolation. If a table ID is specified, LCR will be ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcr
   :type: int


   
   Get or set the Optional load curve describing scale factor on force or moment as a function of relative velocity or rotational velocity. The load curve most define the response in the negative and positive quadrants and pass through point (0,0).
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'S04'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





