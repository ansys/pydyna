





:class:`SetSolidGeneral`
========================


.. py:class:: set_solid_general.SetSolidGeneral(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SOLID_GENERAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetSolidGeneral

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Solid element set ID.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~option`
            - Get or set the OPTION.EQ.ALL: All solid elements will be included in the set,
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

    from set_solid_general import SetSolidGeneral

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Solid element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: str


   
   Get or set the OPTION.EQ.ALL: All solid elements will be included in the set,
   OPTION.EQ.ELEM: Solid elements E1...E7 will be included in the current set,
   OPTION.EQ.DELEM: Solid elements E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from parts E1...E7 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from parts E1...E7 previously added will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside boxes E1...E7 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside boxes E1...E7 previously added will be excluded from the current set.
   OPTION.EQ.SALECPT:Elements inside a box in Structured ALE mesh.E1 here is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6,and E7 correspond to IMIN, IMAX, JMIN, JMAX, KMIN,and KMAX.They are the minimumand the maximum nodal indices along each direction in S - ALE mesh.This option is only to be used for Structured ALE mesh.It can be used with SALEFAC but should not be used in a mixed manner with other “_‌GENERAL” options.
   OPTION.EQ.SALEFAC:Elements on the face of Structured ALE mesh.E1 here is the S - ALE mesh ID(MSHID).E2, E3, E4, E5, E6, and E7 correspond to - X, +X, -Y, +Y, -Z, and +Z faces.Assigning 1 to these 6 values would include all the boundary elements at these faces in the solid element set.This option is only to be used for Structured ALE mesh.It can be used with SALECPT but should not be used in a mixed manner with other “_GENERAL” options.
















   ..
       !! processed by numpydoc !!

.. py:property:: e1
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E1 not used,
   OPTION.EQ.ELEM: Solid element E1 will be included in the current set,
   OPTION.EQ.DELEM: Solid element E1 will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from part E1 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from part E1 will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside box E1 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside box E1 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e2
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E2 not used,
   OPTION.EQ.ELEM: Solid element E2 will be included in the current set,
   OPTION.EQ.DELEM: Solid element E2 will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from part E2 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from part E2 will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside box E2 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside box E2 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e3
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E3 not used,
   OPTION.EQ.ELEM: Solid element E3 will be included in the current set,
   OPTION.EQ.DELEM: Solid element E3 will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from part E3 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from part E3 will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside box E3 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside box E3 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e4
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E4 not used,
   OPTION.EQ.ELEM: Solid element E4 will be included in the current set,
   OPTION.EQ.DELEM: Solid element E4 will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from part E4 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from part E4 will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside box E4 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside box E4 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e5
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E5 not used,
   OPTION.EQ.ELEM: Solid element E5 will be included in the current set,
   OPTION.EQ.DELEM: Solid element E5 will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from part E5 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from part E5 will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside box E5 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside box E5 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e6
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E6 not used,
   OPTION.EQ.ELEM: Solid element E6 will be included in the current set,
   OPTION.EQ.DELEM: Solid element E6 will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from part E6 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from part E6 will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside box E6 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside box E6 will be excluded from the current set.
















   ..
       !! processed by numpydoc !!

.. py:property:: e7
   :type: Optional[int]


   
   Get or set the OPTION.EQ.ALL: E7 not used,
   OPTION.EQ.ELEM: Solid element E7 will be included in the current set,
   OPTION.EQ.DELEM: Solid element E7 will be excluded from the current set,
   OPTION.EQ.PART: Solid elements from part E7 will be included in the current set,
   OPTION.EQ.DPART: Solid elements from part E7 will be excluded from the current set,
   OPTION.EQ.BOX: Solid elements inside box E7 will be included in the current set,
   OPTION.EQ.DBOX: Solid elements inside box E7 will be excluded from the current set.
















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
   :value: 'SOLID_GENERAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





