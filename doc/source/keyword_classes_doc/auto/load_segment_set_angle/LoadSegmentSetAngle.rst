





:class:`LoadSegmentSetAngle`
============================


.. py:class:: load_segment_set_angle.LoadSegmentSetAngle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEGMENT_SET_ANGLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSegmentSetAngle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Loading ID
          * - :py:attr:`~ssid`
            - Get or set the Segment set ID
          * - :py:attr:`~lcid`
            - Get or set the Load curve or function ID defining the traction as a function of the angle.  If IOPT=0 below, define the abscissa between 0 and 2??radians or 0 and 360 degrees if IOPD=1.
          * - :py:attr:`~sf`
            - Get or set the Scale factor on value of the load curve or function.
          * - :py:attr:`~ioptp`
            - Get or set the Flag for periodicity. The default (IOPTP=0) requires the load curve to be defined between 0 and 2?. This is useful, for example, for modeling an engine that is running at a steady state since each rotation will experience the same loading. To model a transient response, IOPTP=1 uses a load curve defined over the full range of angles, permitting a different response on the second and subsequent revolutions.
          * - :py:attr:`~ioptd`
            - Get or set the Flag for specifying if the load curve or function argument is in radians (IOPTD=0, the default) or degrees (IOPTD=1).
          * - :py:attr:`~n1`
            - Get or set the The node specifying the tail of the rotating vector
          * - :py:attr:`~n2`
            - Get or set the The node specifying the head of the rotating vector
          * - :py:attr:`~na`
            - Get or set the The node specifying the head of the vector defining the axis of rotation. The node N1 specifies the tail.
          * - :py:attr:`~ni`
            - Get or set the The node specifying the orientation of the vector at an angle of zero. If the initial angle is zero, NI should be equal to N2.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from load_segment_set_angle import LoadSegmentSetAngle

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve or function ID defining the traction as a function of the angle.  If IOPT=0 below, define the abscissa between 0 and 2??radians or 0 and 360 degrees if IOPD=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor on value of the load curve or function.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioptp
   :type: int


   
   Get or set the Flag for periodicity. The default (IOPTP=0) requires the load curve to be defined between 0 and 2?. This is useful, for example, for modeling an engine that is running at a steady state since each rotation will experience the same loading. To model a transient response, IOPTP=1 uses a load curve defined over the full range of angles, permitting a different response on the second and subsequent revolutions.
















   ..
       !! processed by numpydoc !!

.. py:property:: ioptd
   :type: int


   
   Get or set the Flag for specifying if the load curve or function argument is in radians (IOPTD=0, the default) or degrees (IOPTD=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the The node specifying the tail of the rotating vector
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the The node specifying the head of the rotating vector
















   ..
       !! processed by numpydoc !!

.. py:property:: na
   :type: Optional[int]


   
   Get or set the The node specifying the head of the vector defining the axis of rotation. The node N1 specifies the tail.
















   ..
       !! processed by numpydoc !!

.. py:property:: ni
   :type: Optional[int]


   
   Get or set the The node specifying the orientation of the vector at an angle of zero. If the initial angle is zero, NI should be equal to N2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEGMENT_SET_ANGLE'






