





:class:`AleMapping`
===================


.. py:class:: ale_mapping.AleMapping(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_MAPPING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleMapping

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ammsid`
            - Get or set the Set ID of ALE multi-material groups defined in *SET_‌MULTI-MATERIAL_‌GROUP. See Remark 3.
          * - :py:attr:`~rw`
            - Get or set the Flag defining if the keyword reads or writes in the mapping:
          * - :py:attr:`~ntim`
            - Get or set the For RW = -1:
          * - :py:attr:`~tbeg`
            - Get or set the For RW = -1:
          * - :py:attr:`~tend`
            - Get or set the For RW = -1:
          * - :py:attr:`~vecid`
            - Get or set the For RW = -1:
          * - :py:attr:`~angle`
            - Get or set the For RW = -1:
          * - :py:attr:`~xp`
            - Get or set the For RW = -1:
          * - :py:attr:`~yp`
            - Get or set the For RW = -1:
          * - :py:attr:`~zp`
            - Get or set the For RW = -1:
          * - :py:attr:`~id`
            - Get or set the Part ID or part set ID or element set ID. See Remark 8.
          * - :py:attr:`~type`
            - Get or set the Type of “ID” (see Remark 8):
          * - :py:attr:`~nvol`
            - Get or set the Number of volumes in which the elements are selected for the mapping. See Remark 8.
          * - :py:attr:`~voltyp`
            - Get or set the Type of volume containing the selected elements for the mapping.
          * - :py:attr:`~vecid1`
            - Get or set the ID of the local u-axis defined by *DEFINE_‌VECTOR. See Remark 10.
          * - :py:attr:`~dw1`
            - Get or set the Length in the local w-axis direction. See Remark 11..
          * - :py:attr:`~xl`
            - Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
          * - :py:attr:`~yl`
            - Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
          * - :py:attr:`~zl`
            - Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
          * - :py:attr:`~dw2`
            - Get or set the Length in the local w-axis direction (DW2=DW1 by default). See Remark 11.
          * - :py:attr:`~dv2`
            - Get or set the Length in the local v-axis direction (DV2=DV1 by default). See Remark 12.


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

    from ale_mapping import AleMapping

Property detail
---------------

.. py:property:: ammsid
   :type: Optional[int]


   
   Get or set the Set ID of ALE multi-material groups defined in *SET_‌MULTI-MATERIAL_‌GROUP. See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: rw
   :type: int


   
   Get or set the Flag defining if the keyword reads or writes in the mapping:
   EQ.-1: write in the mapping file. See Remark 4.
   GT.0:   read from the mapping file. |RW| defines the rank of *ALE_MAPPING that wrote in the
   mapping file in the previous run if several keywords contributed to the file creation.
   If there was only one keyword (most of the cases), RW=1. See Remark 4..
















   ..
       !! processed by numpydoc !!

.. py:property:: ntim
   :type: int


   
   Get or set the For RW = -1:
   Number of times to write in the mapping file between the times TBEG and TEND. See Remark 5.
   For RW > 0:
   Rank of the data to be read if, during the previous run, a keyword *ALE_MAPPING with RW=-1 wrote several times in the mapping file.
   If there was only one output (most of the cases), NTIM=1. See Remark 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: float


   
   Get or set the For RW = -1:
   Time to start writing in the mapping file (TBEG=ENDTIM by default). See Remark 5.
   For RW > 0:
   Time to map the data from the mapping file (TBEG=0.0 by default). See Remark 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: Optional[float]


   
   Get or set the For RW = -1:
   Time to stop writing in the mapping file. See Remark 5.
   For RW > 0:
   Ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: Optional[int]


   
   Get or set the For RW = -1:
   Ignored
   For RW > 0:
   ID of the symmetric axis defined by *DEFINE_‌VECTOR.
   The 3 first parameters in *DEFINE_‌VECTOR defines the location of the origin of the previous run. See Remarks 6 and 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: float


   
   Get or set the For RW = -1:
   Ignored
   For RW > 0:
   Angle of rotation in degrees around an axis defined by *DEFINE_‌VECTOR for the 3D to 3D mapping. See Remark 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: float


   
   Get or set the For RW = -1:
   Ignored
   For RW > 0:
   -position of a point on a plane used by specific mappings (only for 2D plain strain to 3D mappings). See Remark 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: float


   
   Get or set the For RW = -1:
   Ignored
   For RW > 0:
   - position of a point on a plane used by specific mappings (only for 2D plain strain to 3D mappings). See Remark 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: float


   
   Get or set the For RW = -1:
   Ignored
   For RW > 0:
   - position of a point on a plane used by specific mappings (only for 2D plain strain to 3D mappings). See Remark 7.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Part ID or part set ID or element set ID. See Remark 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Type of “ID” (see Remark 8):
   EQ.0:   part set ID.
   EQ.1:   part ID.
   EQ.2:   element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nvol
   :type: int


   
   Get or set the Number of volumes in which the elements are selected for the mapping. See Remark 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: voltyp
   :type: Optional[int]


   
   Get or set the Type of volume containing the selected elements for the mapping.
   The absolute value of VOLTYP indicates the type of volume and the sign indicates whether the elements to be selected are in or out of the volume.
   The volume depends on geometrical lengths in a local coordinate system defined by orthonormal axis called ,  and . See Remarks 9,10,11,12 and 13.
   Volume Type
   |VOLTYP|.EQ.1:  Trapezoid 3D (See Figure 0-1).
   |VOLTYP|.EQ.2:  Elliptic truncated cone (See Figure 0-2).
   |VOLTYP|.EQ.3:  Ellipsoid (See Figure 0-3).
   In/Out
   VOLTYP.LT.0:    elements outside the volume are selected.
   VOLTYP.GT.0:    elements inside the volume are selected.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid1
   :type: int


   
   Get or set the ID of the local u-axis defined by *DEFINE_‌VECTOR. See Remark 10.
















   ..
       !! processed by numpydoc !!

.. py:property:: dw1
   :type: float


   
   Get or set the Length in the local w-axis direction. See Remark 11..
















   ..
       !! processed by numpydoc !!

.. py:property:: xl
   :type: float


   
   Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
















   ..
       !! processed by numpydoc !!

.. py:property:: yl
   :type: float


   
   Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
















   ..
       !! processed by numpydoc !!

.. py:property:: zl
   :type: float


   
   Get or set the Global -position of a point along a direction parallel to the -axis. See Remarks 10,11,12 and 13.
















   ..
       !! processed by numpydoc !!

.. py:property:: dw2
   :type: float


   
   Get or set the Length in the local w-axis direction (DW2=DW1 by default). See Remark 11.
















   ..
       !! processed by numpydoc !!

.. py:property:: dv2
   :type: float


   
   Get or set the Length in the local v-axis direction (DV2=DV1 by default). See Remark 12.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'MAPPING'






