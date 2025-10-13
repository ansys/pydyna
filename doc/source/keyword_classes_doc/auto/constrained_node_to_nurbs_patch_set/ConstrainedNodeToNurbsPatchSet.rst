





:class:`ConstrainedNodeToNurbsPatchSet`
=======================================


.. py:class:: constrained_node_to_nurbs_patch_set.ConstrainedNodeToNurbsPatchSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_NODE_TO_NURBS_PATCH_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedNodeToNurbsPatchSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~patchid`
            - Get or set the Patch ID.
          * - :py:attr:`~nsid`
            - Get or set the Nodal set ID
          * - :py:attr:`~con`
            - Get or set the Constraint parameter for extra node(s) of NSID.  Its definition is same as that of CON2 when CM0=-1 as described in MAT_RIGID.  For example ‘1110’ means constrained z-translation, x-rotation and y-rotation.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for constraint
          * - :py:attr:`~sf`
            - Get or set the Penalty force scale factor for the penalty-based constraint
          * - :py:attr:`~dbflg`
            - Get or set the Discrete beam flag. If CON = 0 and displacement boundary conditions are applied to nodes specified in NSID, then this flag must be set to 1.


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

    from constrained_node_to_nurbs_patch_set import ConstrainedNodeToNurbsPatchSet

Property detail
---------------

.. py:property:: patchid
   :type: Optional[int]


   
   Get or set the Patch ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Nodal set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: con
   :type: str


   
   Get or set the Constraint parameter for extra node(s) of NSID.  Its definition is same as that of CON2 when CM0=-1 as described in MAT_RIGID.  For example ‘1110’ means constrained z-translation, x-rotation and y-rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for constraint
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Penalty force scale factor for the penalty-based constraint
















   ..
       !! processed by numpydoc !!

.. py:property:: dbflg
   :type: int


   
   Get or set the Discrete beam flag. If CON = 0 and displacement boundary conditions are applied to nodes specified in NSID, then this flag must be set to 1.
   When DBFLG = 1, discrete beam elements are created to connect nodes in NSID to the patch.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'NODE_TO_NURBS_PATCH_SET'






