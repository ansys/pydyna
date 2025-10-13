





:class:`AlePrescribedMotion`
============================


.. py:class:: ale_prescribed_motion.AlePrescribedMotion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_PRESCRIBED_MOTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AlePrescribedMotion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mmsid`
            - Get or set the Multi-Material Set ID (see *SET_‌MULTI-MATERIAL_‌GROUP_‌LIST).
          * - :py:attr:`~inside`
            - Get or set the Flag to define which nodes the motion is prescribed for (see Remark 2):
          * - :py:attr:`~sidr`
            - Get or set the Flag controlling the use of this keyword during dynamic relaxation.
          * - :py:attr:`~lcvtx`
            - Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3.
          * - :py:attr:`~lcvty`
            - Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
          * - :py:attr:`~lcvtz`
            - Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
          * - :py:attr:`~lcvrx`
            - Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
          * - :py:attr:`~lcvry`
            - Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
          * - :py:attr:`~lcvrz`
            - Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
          * - :py:attr:`~xg`
            - Get or set the Position of the rotation center. See Remark 4.
          * - :py:attr:`~yg`
            - Get or set the Position of the rotation center. See Remark 4.
          * - :py:attr:`~zg`
            - Get or set the Position of the rotation center. See Remark 4.


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

    from ale_prescribed_motion import AlePrescribedMotion

Property detail
---------------

.. py:property:: mmsid
   :type: Optional[int]


   
   Get or set the Multi-Material Set ID (see *SET_‌MULTI-MATERIAL_‌GROUP_‌LIST).
















   ..
       !! processed by numpydoc !!

.. py:property:: inside
   :type: int


   
   Get or set the Flag to define which nodes the motion is prescribed for (see Remark 2):
   EQ.0:   Nodes connected to at least one ALE element that is at the minimum partially filled by a group of MMSID
   EQ.1:   Nodes connected to at least one ALE element that is fully filled by a group of MMSID
   EQ.2:   Nodes only connected to ALE elements that are fully filled by a group of MMSID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidr
   :type: int


   
   Get or set the Flag controlling the use of this keyword during dynamic relaxation.
   EQ.0:   the keyword is applied in normal analysis phase only,
   EQ.1:   the keyword is applied in dynamic relaxation phase but not the normal analysis phase,
   EQ.2:   the keyword is applied in both dynamic relaxation phase and normal analysis phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvtx
   :type: Optional[int]


   
   Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvty
   :type: Optional[int]


   
   Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvtz
   :type: Optional[int]


   
   Get or set the Curve IDs for the translation in each global direction; see *DEFINE_‌CURVE.  See Remark 3..
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvrx
   :type: Optional[int]


   
   Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvry
   :type: Optional[int]


   
   Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvrz
   :type: Optional[int]


   
   Get or set the Curve IDs for the rotation around each global direction; see *DEFINE_‌CURVE.  See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: xg
   :type: Optional[float]


   
   Get or set the Position of the rotation center. See Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: yg
   :type: Optional[float]


   
   Get or set the Position of the rotation center. See Remark 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: zg
   :type: Optional[float]


   
   Get or set the Position of the rotation center. See Remark 4.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'PRESCRIBED_MOTION'






