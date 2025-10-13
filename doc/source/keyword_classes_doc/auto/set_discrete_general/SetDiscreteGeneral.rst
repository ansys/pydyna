





:class:`SetDiscreteGeneral`
===========================


.. py:class:: set_discrete_general.SetDiscreteGeneral(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_DISCRETE_GENERAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetDiscreteGeneral

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Discrete element set ID.
          * - :py:attr:`~option`
            - Get or set the OPTION.EQ.ALL: All discrete elements will be included in the set,
          * - :py:attr:`~e1`
            - Get or set the OPTION.EQ.ALL: E1 not used,
          * - :py:attr:`~e2`
            - Get or set the OPTION.EQ.ALL: E2 not used,
          * - :py:attr:`~e3`
            - Get or set the OPTION.EQ.ALL: E3 not used,
          * - :py:attr:`~e4`
            - Get or set the OPTION.EQ.ALL: E4 not used,
          * - :py:attr:`~e5`
            - Get or set the OPTION.EQ.ALL: E5 not used,
          * - :py:attr:`~e6`
            - Get or set the OPTION.EQ.ALL: E6 not used,
          * - :py:attr:`~e7`
            - Get or set the OPTION.EQ.ALL: E7 not used,
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

    from set_discrete_general import SetDiscreteGeneral

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Discrete element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the OPTION.EQ.ALL: All discrete elements will be included in the set,
   OPTION.EQ.ELEM: Discrete elements E1...E7 will be included in the current set,
   OPTION.EQ.DELEM: Discrete elements E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from parts E1...E7 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from parts E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside boxes E1...E7 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside boxes E1...E7 previously added will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E1 not used,
   OPTION.EQ.ELEM: Discrete element E1 will be included in the current set,
   OPTION.EQ.DELEM: Discrete element E1 will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from part E1 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from part E1 will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside box E1 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside box E1 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E2 not used,
   OPTION.EQ.ELEM: Discrete element E2 will be included in the current set,
   OPTION.EQ.DELEM: Discrete element E2 will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from part E2 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from part E2 will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside box E2 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside box E2 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E3 not used,
   OPTION.EQ.ELEM: Discrete element E3 will be included in the current set,
   OPTION.EQ.DELEM: Discrete element E3 will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from part E3 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from part E3 will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside box E3 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside box E3 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E4 not used,
   OPTION.EQ.ELEM: Discrete element E4 will be included in the current set,
   OPTION.EQ.DELEM: Discrete element E4 will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from part E4 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from part E4 will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside box E4 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside box E4 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E5 not used,
   OPTION.EQ.ELEM: Discrete element E5 will be included in the current set,
   OPTION.EQ.DELEM: Discrete element E5 will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from part E5 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from part E5 will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside box E5 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside box E5 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e6
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E6 not used,
   OPTION.EQ.ELEM: Discrete element E6 will be included in the current set,
   OPTION.EQ.DELEM: Discrete element E6 will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from part E6 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from part E6 will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside box E6 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside box E6 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e7
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E7 not used,
   OPTION.EQ.ELEM: Discrete element E7 will be included in the current set,
   OPTION.EQ.DELEM: Discrete element E7 will be excluded from the current set,
   OPTION.EQ.PART: Discrete elements from part E7 will be included in the current set,
   OPTION.EQ.DPART: Discrete elements from part E7 will be excluded from the current set,
   OPTION.EQ.BOX: Discrete elements inside box E7 will be included in the current set,
   OPTION.EQ.DBOX: Discrete elements inside box E7 will be excluded from the current set.
















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
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'DISCRETE_GENERAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





