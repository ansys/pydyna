





:class:`SetShellGeneralCollect`
===============================


.. py:class:: set_shell_general_collect.SetShellGeneralCollect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SHELL_GENERAL_COLLECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetShellGeneralCollect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Shell element set ID. All shell sets should have a unique set ID.
          * - :py:attr:`~da1`
            - Get or set the First attribute default value is 0.0.
          * - :py:attr:`~da2`
            - Get or set the Second attribute default value is 0.0.
          * - :py:attr:`~da3`
            - Get or set the Third attribute default value is 0.0.
          * - :py:attr:`~da4`
            - Get or set the Fourth attribute default value is 0.0.
          * - :py:attr:`~option`
            - Get or set the OPTION.EQ.ALL: All shell elements will be included in the set,
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

    from set_shell_general_collect import SetShellGeneralCollect

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Shell element set ID. All shell sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: da1
   :type: float


   
   Get or set the First attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da2
   :type: float


   
   Get or set the Second attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da3
   :type: float


   
   Get or set the Third attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: da4
   :type: float


   
   Get or set the Fourth attribute default value is 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the OPTION.EQ.ALL: All shell elements will be included in the set,
   OPTION.EQ.ELEM: Shell elements E1...E7 will be included in the current set,
   OPTION.EQ.DELEM: Shell elements E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from parts E1...E7 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from parts E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside boxes E1...E7 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside boxes E1...E7 previously added will be excluded from the current set.
   OPTION.EQ.SALECPT:Elements inside a box for a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4,and E5 correspond to IMIN, IMAX, JMIN,and JMAX, respectively.They are the minimumand the maximum nodal indices along each direction in the S - ALE mesh.This option is only to be used for a Structured ALE mesh.It can be used with SALEFAC to generate a shell set but should not be used with other “_‌GENERAL” options.
   OPTION.EQ.SALEFAC:Elements on the face of a 2D Structured ALE mesh.E1 is the S - ALE mesh ID(MSHID).E2, E3, E4, and E5 correspond to the - X, +X, -Y, and +Y faces, respectively.Assigning 1 to these 4 values would include all the boundary elements at these faces in the shell element set.This option is only to be used for a Structured ALE mesh.It can be used with SALECPT to generate a shell set but should not be used with other “_GENERAL” options.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E1 not used,
   OPTION.EQ.ELEM: Shell element E1 will be included in the current set,
   OPTION.EQ.DELEM: Shell element E1 will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from part E1 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from part E1 will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside box E1 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside box E1 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E2 not used,
   OPTION.EQ.ELEM: Shell element E2 will be included in the current set,
   OPTION.EQ.DELEM: Shell element E2 will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from part E2 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from part E2 will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside box E2 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside box E2 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E3 not used,
   OPTION.EQ.ELEM: Shell element E3 will be included in the current set,
   OPTION.EQ.DELEM: Shell element E3 will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from part E3 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from part E3 will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside box E3 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside box E3 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E4 not used,
   OPTION.EQ.ELEM: Shell element E4 will be included in the current set,
   OPTION.EQ.DELEM: Shell element E4 will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from part E4 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from part E4 will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside box E4 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside box E4 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E5 not used,
   OPTION.EQ.ELEM: Shell element E5 will be included in the current set,
   OPTION.EQ.DELEM: Shell element E5 will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from part E5 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from part E5 will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside box E5 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside box E5 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e6
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E6 not used,
   OPTION.EQ.ELEM: Shell element E6 will be included in the current set,
   OPTION.EQ.DELEM: Shell element E6 will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from part E6 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from part E6 will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside box E6 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside box E6 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e7
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E7 not used,
   OPTION.EQ.ELEM: Shell element E7 will be included in the current set,
   OPTION.EQ.DELEM: Shell element E7 will be excluded from the current set,
   OPTION.EQ.PART: Shell elements from part E7 will be included in the current set,
   OPTION.EQ.DPART: Shell elements from part E7 will be excluded from the current set,
   OPTION.EQ.BOX: Shell elements inside box E7 will be included in the current set,
   OPTION.EQ.DBOX: Shell elements inside box E7 will be excluded from the current set.
















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
   :value: 'SHELL_GENERAL_COLLECT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





