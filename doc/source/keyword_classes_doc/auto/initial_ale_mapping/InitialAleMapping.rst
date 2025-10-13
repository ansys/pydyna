





:class:`InitialAleMapping`
==========================


.. py:class:: initial_ale_mapping.InitialAleMapping(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_ALE_MAPPING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialAleMapping

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID or part set ID
          * - :py:attr:`~typ`
            - Get or set the Type of “PID” (see remark 1):
          * - :py:attr:`~ammsid`
            - Get or set the Set ID of ALE multi-material groups defined in *SET_MULTI-MATERIAL_GROUP
          * - :py:attr:`~xo`
            - Get or set the Origin position in global X-direction
          * - :py:attr:`~yo`
            - Get or set the Origin position in global Y-direction
          * - :py:attr:`~zo`
            - Get or set the Origin position in global Z-direction
          * - :py:attr:`~vecid`
            - Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR
          * - :py:attr:`~angle`
            - Get or set the Angle of rotation in degrees around an axis defined by *DEFINE_VECTOR for the 3D to 3D mapping. See Remark 4
          * - :py:attr:`~sym`
            - Get or set the Treatment of the elements and nodes that are out of the mapping bounds (meaning the coordinates of their projections on the previous mesh are outside this mesh).
          * - :py:attr:`~tbeg`
            - Get or set the Time to start the run. It replaces the termination time of the previous run that generated the mapping file if TBEG is the larger


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

    from initial_ale_mapping import InitialAleMapping

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID or part set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: typ
   :type: int


   
   Get or set the Type of “PID” (see remark 1):
   EQ.0:  part set ID (PSID).
   EQ.1:  part ID (PID)
















   ..
       !! processed by numpydoc !!

.. py:property:: ammsid
   :type: Optional[int]


   
   Get or set the Set ID of ALE multi-material groups defined in *SET_MULTI-MATERIAL_GROUP
















   ..
       !! processed by numpydoc !!

.. py:property:: xo
   :type: float


   
   Get or set the Origin position in global X-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: yo
   :type: float


   
   Get or set the Origin position in global Y-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: zo
   :type: float


   
   Get or set the Origin position in global Z-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: Optional[int]


   
   Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: Optional[float]


   
   Get or set the Angle of rotation in degrees around an axis defined by *DEFINE_VECTOR for the 3D to 3D mapping. See Remark 4
















   ..
       !! processed by numpydoc !!

.. py:property:: sym
   :type: int


   
   Get or set the Treatment of the elements and nodes that are out of the mapping bounds (meaning the coordinates of their projections on the previous mesh are outside this mesh).
   SYM is a 6-digit parameter. Each digit represents a plane for a box that encloses the previous mesh.
   These planes are parallel to the previous coordinate system:
   EQ.00000p: Rule for the X - plane along the lower previous mesh bound
   EQ.0000p0 : Rule for the X - plane along the upper previous mesh bound
   EQ.000p00 : Rule for the Y - plane along the lower previous mesh bound
   EQ.00p000 : Rule for the Y - plane along the upper previous mesh bound
   EQ.0p0000 : Rule for the Z - plane along the lower previous mesh bound
   EQ.p00000 : Rule for the Z - plane along the upper previous mesh bound
   The value of p defines the rule to apply in relation to the box plane :
   EQ.0 : Do nothing.
   EQ.1 : Translational symmetry(direction of translation orthogonal to the box plane)
   EQ.2 : Mirror - image symmetry about the box plane
   EQ.3 : Continuity of boundary elements and nodes along the box plane
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: float


   
   Get or set the Time to start the run. It replaces the termination time of the previous run that generated the mapping file if TBEG is the larger
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'ALE_MAPPING'






