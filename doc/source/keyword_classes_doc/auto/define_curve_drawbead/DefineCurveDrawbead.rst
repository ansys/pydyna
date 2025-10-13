





:class:`DefineCurveDrawbead`
============================


.. py:class:: define_curve_drawbead.DefineCurveDrawbead(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_DRAWBEAD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveDrawbead

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Curve ID.
          * - :py:attr:`~tcype`
            - Get or set the Bea data type.
          * - :py:attr:`~vid`
            - Get or set the Vector ID, See DEFINE_VECTOR. This vector is used to project the bead to the rigid part (PID)
          * - :py:attr:`~pid`
            - Get or set the Part ID to attach the drawbead.
          * - :py:attr:`~blkid`
            - Get or set the The part id of the blank.
          * - :py:attr:`~perct`
            - Get or set the Percentage of restraining force (the ratio of restraining force over Lock force). The value should be between 0 and 100
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

    from define_curve_drawbead import DefineCurveDrawbead

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: tcype
   :type: int


   
   Get or set the Bea data type.
   EQ.1:x,y,z data
   EQ. 2. IGES data
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID, See DEFINE_VECTOR. This vector is used to project the bead to the rigid part (PID)
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID to attach the drawbead.
















   ..
       !! processed by numpydoc !!

.. py:property:: blkid
   :type: Optional[int]


   
   Get or set the The part id of the blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: perct
   :type: Optional[int]


   
   Get or set the Percentage of restraining force (the ratio of restraining force over Lock force). The value should be between 0 and 100
















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
   :value: 'CURVE_DRAWBEAD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





