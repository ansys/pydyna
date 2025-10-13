





:class:`AleStructuredMeshControlPoints`
=======================================


.. py:class:: ale_structured_mesh_control_points.AleStructuredMeshControlPoints(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_STRUCTURED_MESH_CONTROL_POINTS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleStructuredMeshControlPoints

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cpid`
            - Get or set the Control Points ID. A unique number must be specified. This ID is to be
          * - :py:attr:`~icase`
            - Get or set the A flag to trigger special logic for a more user-friendly input format for progressive mesh spacing. Please see examples sections below on ICASE usage.
          * - :py:attr:`~sfo`
            - Get or set the Scale factor for ordinate value. This is useful for simple modifications.        EQ.0.0: default set to 1.0.
          * - :py:attr:`~offo`
            - Get or set the Offset for ordinate values. See Remark 1.
          * - :py:attr:`~n`
            - Get or set the Control point node number.
          * - :py:attr:`~x`
            - Get or set the Control point position.
          * - :py:attr:`~ratio`
            - Get or set the Ratio for progressive mesh spacing.  Progressively larger or smaller mesh will be generated between the control point that has nonzero ratio specified and the control point following it.  See remark 2.


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

    from ale_structured_mesh_control_points import AleStructuredMeshControlPoints

Property detail
---------------

.. py:property:: cpid
   :type: int


   
   Get or set the Control Points ID. A unique number must be specified. This ID is to be
   referred in the three fields marked up CPIDX, CPIDY, CPIDZ in *ALE_     STRUCTURED_MESH.
















   ..
       !! processed by numpydoc !!

.. py:property:: icase
   :type: int


   
   Get or set the A flag to trigger special logic for a more user-friendly input format for progressive mesh spacing. Please see examples sections below on ICASE usage.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfo
   :type: float


   
   Get or set the Scale factor for ordinate value. This is useful for simple modifications.        EQ.0.0: default set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: offo
   :type: float


   
   Get or set the Offset for ordinate values. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: int


   
   Get or set the Control point node number.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Control point position.
















   ..
       !! processed by numpydoc !!

.. py:property:: ratio
   :type: float


   
   Get or set the Ratio for progressive mesh spacing.  Progressively larger or smaller mesh will be generated between the control point that has nonzero ratio specified and the control point following it.  See remark 2.
   GT.0.0: mesh size increases; dl(n+1)=dl_n*(1+ratio)
   LT.0.0: mesh size decreases; dl(n+1)=dl_n/(1-ratio).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'STRUCTURED_MESH_CONTROL_POINTS'






