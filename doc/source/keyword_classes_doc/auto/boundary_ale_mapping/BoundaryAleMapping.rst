





:class:`BoundaryAleMapping`
===========================


.. py:class:: boundary_ale_mapping.BoundaryAleMapping(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ALE_MAPPING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAleMapping

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Part ID or part set ID or element set ID
          * - :py:attr:`~typ`
            - Get or set the Type of "ID" (see remark 1):
          * - :py:attr:`~ammsid`
            - Get or set the Set ID of ALE multi-material groups defined in *SET_MULTI-MA-TERIAL_GROUP
          * - :py:attr:`~ivoltyp`
            - Get or set the Type of volume containing the selected elements for the mapping.
          * - :py:attr:`~birth`
            - Get or set the Birth time to write or read the mapping file. If a mapping file is
          * - :py:attr:`~death`
            - Get or set the Death time to write or read the mapping file. If a mapping file is
          * - :py:attr:`~dtout`
            - Get or set the Time interval between outputs in the mapping file. This parameter
          * - :py:attr:`~ini`
            - Get or set the Flag to initialize all the ALE domain of the next run:
          * - :py:attr:`~thick`
            - Get or set the Thickness for the element selection using surfaces.
          * - :py:attr:`~radius`
            - Get or set the Radius for abs(IVOLTYP) = 1 and abs(IVOLTYP) = 2.
          * - :py:attr:`~x1`
            - Get or set the If abs(IVOLTYP).EQ.1: X1 is the X-coordinate of the sphere center
          * - :py:attr:`~y1`
            - Get or set the If abs(IVOLTYP).EQ.1: Y1 is the Y-coordinate of the sphere center
          * - :py:attr:`~z1`
            - Get or set the If abs(IVOLTYP).EQ.1: Z1 is the Z-coordinate of the sphere center
          * - :py:attr:`~x2`
            - Get or set the If abs(IVOLTYP).EQ.1: ignored
          * - :py:attr:`~y2`
            - Get or set the If abs(IVOLTYP).EQ.1: ignored
          * - :py:attr:`~z2`
            - Get or set the If abs(IVOLTYP).EQ.1: ignored
          * - :py:attr:`~x0`
            - Get or set the Origin position in global X-direction. See remark 2.
          * - :py:attr:`~y0`
            - Get or set the Origin position in global Y-direction. See remark 2.
          * - :py:attr:`~z0`
            - Get or set the Origin position in global Z-direction. See remark 2.
          * - :py:attr:`~vecid`
            - Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR. See remark 3.


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

    from boundary_ale_mapping import BoundaryAleMapping

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Part ID or part set ID or element set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: typ
   :type: int


   
   Get or set the Type of "ID" (see remark 1):
   EQ.0: part set ID.
   EQ.1: part ID.
   EQ.2: shell set ID.
   EQ.3: solid set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ammsid
   :type: Optional[int]


   
   Get or set the Set ID of ALE multi-material groups defined in *SET_MULTI-MA-TERIAL_GROUP
















   ..
       !! processed by numpydoc !!

.. py:property:: ivoltyp
   :type: Optional[int]


   
   Get or set the Type of volume containing the selected elements for the mapping.
   The absolute value of IVOLTYPE indicates the type of volume and
   the sign indicates whether the data is being read of written. Volume Type
   |IVOLTYP|.EQ.1: Spherical surface with thickness (THICK).
   |IVOLTYP|.EQ.2: Box.
   |IVOLTYP|.EQ.3: Cylindrical surface with thickness (THICK)
   |IVOLTYP|.EQ.4: All the elements defined by ID. Read/Write
   IVOLTYP.LT.0: data from the mapping file are read for the elements of this volume.
   IVOLTYP.GT.0: data from the elements of this volume are written in the mapping file.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Birth time to write or read the mapping file. If a mapping file is
   written, the next run reading this file will begin at time BIRTH if this parameter for this next run is not larger.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Death time to write or read the mapping file. If a mapping file is
   written, the next run will stop to read this file at time DEATH if this parameter for this next run is not smaller.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Time interval between outputs in the mapping file. This parameter
   is only used to write in the mapping file
















   ..
       !! processed by numpydoc !!

.. py:property:: ini
   :type: int


   
   Get or set the Flag to initialize all the ALE domain of the next run:
   EQ.0: No initialization
   EQ.1: Initialization. *INITIAL_ALE_MAPPING will have to be in
   the input deck of the next run to read the data from the
   mapping file. The initial time of the next run will be BIRTH
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: float


   
   Get or set the Thickness for the element selection using surfaces.
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: float


   
   Get or set the Radius for abs(IVOLTYP) = 1 and abs(IVOLTYP) = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: float


   
   Get or set the If abs(IVOLTYP).EQ.1: X1 is the X-coordinate of the sphere center
   If abs(IVOLTYP).EQ.2: X1 is the X-coordinate of the boxs minimum point.
   If abs(IVOLTYP).EQ.3: X1 is the X-coordinate of a point on the cylinders axis.
   If abs(IVOLTYP).EQ.4: ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: float


   
   Get or set the If abs(IVOLTYP).EQ.1: Y1 is the Y-coordinate of the sphere center
   If abs(IVOLTYP).EQ.2: Y1 is the Y-coordinate of the boxs minimum point.
   If abs(IVOLTYP).EQ.3: Y1 is the Y-coordinate of a point on the cylinders axis.
   If abs(IVOLTYP).EQ.4: ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: float


   
   Get or set the If abs(IVOLTYP).EQ.1: Z1 is the Z-coordinate of the sphere center
   If abs(IVOLTYP).EQ.2: Z1 is the Z-coordinate of the boxs minimum point.
   If abs(IVOLTYP).EQ.3: Z1 is the Z-coordinate of a point on the cylinders axis.
   If abs(IVOLTYP).EQ.4: ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: float


   
   Get or set the If abs(IVOLTYP).EQ.1: ignored
   If abs(IVOLTYP).EQ.2: X2 is the X-coordinate of the boxs maximum point.
   If abs(IVOLTYP).EQ.3: X2 is the X-coordinate of a vector parallel to the cylinders axis..
   If abs(IVOLTYP).EQ.4: ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: float


   
   Get or set the If abs(IVOLTYP).EQ.1: ignored
   If abs(IVOLTYP).EQ.2: Y2 is the Y-coordinate of the boxs maximum point.
   If abs(IVOLTYP).EQ.3: Y2 is the Y-coordinate of a vector parallel to the cylinders axis..
   If abs(IVOLTYP).EQ.4: ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: float


   
   Get or set the If abs(IVOLTYP).EQ.1: ignored
   If abs(IVOLTYP).EQ.2: Z2 is the X-coordinate of the boxs maximum point.
   If abs(IVOLTYP).EQ.3: Z2 is the Z-coordinate of a vector parallel to the cylinders axis..
   If abs(IVOLTYP).EQ.4: ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: float


   
   Get or set the Origin position in global X-direction. See remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: y0
   :type: float


   
   Get or set the Origin position in global Y-direction. See remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: z0
   :type: float


   
   Get or set the Origin position in global Z-direction. See remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: vecid
   :type: Optional[int]


   
   Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR. See remark 3.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ALE_MAPPING'






