





:class:`DefineBoxNodesAdaptive`
===============================


.. py:class:: define_box_nodes_adaptive.DefineBoxNodesAdaptive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_BOX_NODES_ADAPTIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineBoxNodesAdaptive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~boxid`
            - Get or set the Box ID. Define unique numbers.
          * - :py:attr:`~node`
            - Get or set the Part ID of blank.
          * - :py:attr:`~lcx`
            - Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
          * - :py:attr:`~lcy`
            - Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
          * - :py:attr:`~lcz`
            - Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
          * - :py:attr:`~itype`
            - Get or set the Type of curves LCX, LCY and LCZ.  Currently only time as a function of displacement load curves are supported.
          * - :py:attr:`~radius`
            - Get or set the The radius of the tube that defines the fission/fusion boundary.
          * - :py:attr:`~npiece`
            - Get or set the Number of segments used to approximate the tool path in one adaptive step.  Note that the tool’s path is divided into several linear segments for approximation
          * - :py:attr:`~pid`
            - Get or set the The deformable part or part set ID on which the tube adaptivity is to be applied (see *PART).
          * - :py:attr:`~level`
            - Get or set the Desired mesh refinement level. Level set to a value of 1, 2, 3, … allows a maximum of 1, 4, 16, … elements to be created for each original element in the “tube region”.
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

    from define_box_nodes_adaptive import DefineBoxNodesAdaptive

Property detail
---------------

.. py:property:: boxid
   :type: int


   
   Get or set the Box ID. Define unique numbers.
















   ..
       !! processed by numpydoc !!

.. py:property:: node
   :type: int


   
   Get or set the Part ID of blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcx
   :type: Optional[int]


   
   Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: lcy
   :type: Optional[int]


   
   Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: lcz
   :type: Optional[int]


   
   Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Type of curves LCX, LCY and LCZ.  Currently only time as a function of displacement load curves are supported.
   EQ.2:   LCX, LCYand LCZ are defined as time as a function of displacement..
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: float


   
   Get or set the The radius of the tube that defines the fission/fusion boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: npiece
   :type: int


   
   Get or set the Number of segments used to approximate the tool path in one adaptive step.  Note that the tool’s path is divided into several linear segments for approximation
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the The deformable part or part set ID on which the tube adaptivity is to be applied (see *PART).
   GT.0:   Part ID
   LT.0 : | PID | is
   a part set ID.A part set ID can be useful for simulating the forming of tailor welded blanks.
















   ..
       !! processed by numpydoc !!

.. py:property:: level
   :type: Optional[int]


   
   Get or set the Desired mesh refinement level. Level set to a value of 1, 2, 3, … allows a maximum of 1, 4, 16, … elements to be created for each original element in the “tube region”.
















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
   :value: 'BOX_NODES_ADAPTIVE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





