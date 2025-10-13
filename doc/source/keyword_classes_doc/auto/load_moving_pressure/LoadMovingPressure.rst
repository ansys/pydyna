





:class:`LoadMovingPressure`
===========================


.. py:class:: load_moving_pressure.LoadMovingPressure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_MOVING_PRESSURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadMovingPressure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~loadid`
            - Get or set the Loading ID
          * - :py:attr:`~node1`
            - Get or set the Node located at the origin of the nozzle
          * - :py:attr:`~node2`
            - Get or set the Node located at the head of the nozzle
          * - :py:attr:`~lcid`
            - Get or set the Load curve or function (see *DEFINE_FUNCTION) ID defining pressure versus radial distance from the center of the jet
          * - :py:attr:`~cutoff`
            - Get or set the Outer radius of jet.  The pressure acting outside this radius is set to zero
          * - :py:attr:`~lcidt`
            - Get or set the Load curve or function (see *DEFINE_FUNCTION) ID, which scales the pressure as a function of time.
          * - :py:attr:`~lcidd`
            - Get or set the Load curve or function (see *DEFINE_FUNCTION) ID, which scales the pressure as a function of distance from the nozzle.
          * - :py:attr:`~idir`
            - Get or set the Value that determines the direction of the pressure applied on the segments (see Remark 1)
          * - :py:attr:`~lsflg`
            - Get or set the Line-of-sight flag
          * - :py:attr:`~id`
            - Get or set the Segment set ID, shell element set ID, part set ID, or part ID.  See IDT below
          * - :py:attr:`~idtype`
            - Get or set the Value that determines the meaning of variable ID:
          * - :py:attr:`~nip`
            - Get or set the Number of integration in segment used to compute pressure loads.


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

    from load_moving_pressure import LoadMovingPressure

Property detail
---------------

.. py:property:: loadid
   :type: Optional[int]


   
   Get or set the Loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: node1
   :type: Optional[int]


   
   Get or set the Node located at the origin of the nozzle
















   ..
       !! processed by numpydoc !!

.. py:property:: node2
   :type: Optional[int]


   
   Get or set the Node located at the head of the nozzle
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve or function (see *DEFINE_FUNCTION) ID defining pressure versus radial distance from the center of the jet
















   ..
       !! processed by numpydoc !!

.. py:property:: cutoff
   :type: Optional[float]


   
   Get or set the Outer radius of jet.  The pressure acting outside this radius is set to zero
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidt
   :type: Optional[int]


   
   Get or set the Load curve or function (see *DEFINE_FUNCTION) ID, which scales the pressure as a function of time.
   If a load curve isn't specified, the scale factor defaults to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidd
   :type: Optional[int]


   
   Get or set the Load curve or function (see *DEFINE_FUNCTION) ID, which scales the pressure as a function of distance from the nozzle.
   If a load curve isn't specified, the scale factor defaults to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: idir
   :type: int


   
   Get or set the Value that determines the direction of the pressure applied on the segments (see Remark 1)
   EQ.0:   the normal direction of the segments
   EQ.1 : the direction from the nozzle(NODE1) to the segments
   EQ.2 : the direction from NODE1 to NODE2
   EQ.3 : pressure is in the direction from NODE1 to NODE2 but only the normal component is applied on the segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: lsflg
   :type: int


   
   Get or set the Line-of-sight flag
   EQ.0:   see Remark 2
   EQ.1 : pressure is applied on the first - hit segments from the nozzle
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Segment set ID, shell element set ID, part set ID, or part ID.  See IDT below
















   ..
       !! processed by numpydoc !!

.. py:property:: idtype
   :type: int


   
   Get or set the Value that determines the meaning of variable ID:
   EQ.0:   ID is a segment set ID,
   EQ.1:   ID is a shell set ID,
   EQ.2:   ID is a part set ID,
   EQ.3:   ID is a part ID,
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: Optional[int]


   
   Get or set the Number of integration in segment used to compute pressure loads.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'MOVING_PRESSURE'






