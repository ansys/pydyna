





:class:`ElementBeamSectionOffset`
=================================


.. py:class:: element_beam_section_offset.ElementBeamSectionOffset(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_BEAM_SECTION_OFFSET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementBeamSectionOffset

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Element ID. A unique number must be used.
          * - :py:attr:`~pid`
            - Get or set the Part ID.
          * - :py:attr:`~n1`
            - Get or set the Nodal point 1.
          * - :py:attr:`~n2`
            - Get or set the Nodal point 2. This node is optional for the spot weld, beam type 9, since if it not defined it will be created automatically and given a nonconfliciting nodal point ID. Nodes N1 and N2 are automatically positioned for the spot weld beam element.
          * - :py:attr:`~n3`
            - Get or set the Nodal point 3, the third node, N3, is optional for beam type 3,6,7,8,and 9, if the latter, type 9, has a non-circular cross section. The third node is used for the discrete beam, type 6, if and only if SCOOR is set to 2.0 in the *SECTION_BEAM input, but even in this case it is optional.
          * - :py:attr:`~rt1`
            - Get or set the Release conditions for translations at node N1.
          * - :py:attr:`~rr1`
            - Get or set the Release conditions for rotations at node N1.
          * - :py:attr:`~rt2`
            - Get or set the Release conditions for translations at node N2.
          * - :py:attr:`~rr2`
            - Get or set the Release conditions for rotations at node N2.
          * - :py:attr:`~local`
            - Get or set the Coordinate system
          * - :py:attr:`~stype`
            - Get or set the Section type (A format):
          * - :py:attr:`~d1`
            - Get or set the Input parameters for section option using STYPE above
          * - :py:attr:`~d2`
            - Get or set the Input parameters for section option using STYPE above
          * - :py:attr:`~d3`
            - Get or set the Input parameters for section option using STYPE above
          * - :py:attr:`~d4`
            - Get or set the Input parameters for section option using STYPE above
          * - :py:attr:`~d5`
            - Get or set the Input parameters for section option using STYPE above
          * - :py:attr:`~d6`
            - Get or set the Input parameters for section option using STYPE above
          * - :py:attr:`~wx1`
            - Get or set the Offset vector at modal point N1.
          * - :py:attr:`~wy1`
            - Get or set the Offset vector at modal point N1.
          * - :py:attr:`~wz1`
            - Get or set the Offset vector at modal point N1.
          * - :py:attr:`~wx2`
            - Get or set the Offset vector at modal point N2.
          * - :py:attr:`~wy2`
            - Get or set the Offset vector at modal point N2.
          * - :py:attr:`~wz2`
            - Get or set the Offset vector at modal point N2.


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

    from element_beam_section_offset import ElementBeamSectionOffset

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID. A unique number must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Nodal point 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Nodal point 2. This node is optional for the spot weld, beam type 9, since if it not defined it will be created automatically and given a nonconfliciting nodal point ID. Nodes N1 and N2 are automatically positioned for the spot weld beam element.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Nodal point 3, the third node, N3, is optional for beam type 3,6,7,8,and 9, if the latter, type 9, has a non-circular cross section. The third node is used for the discrete beam, type 6, if and only if SCOOR is set to 2.0 in the *SECTION_BEAM input, but even in this case it is optional.
















   ..
       !! processed by numpydoc !!

.. py:property:: rt1
   :type: int


   
   Get or set the Release conditions for translations at node N1.
   EQ.0: no translational degrees-of-freedom are released,
   EQ.1: x-translational degree-of-freedom,
   EQ.2: y-translational degree-of-freedom,
   EQ.3: z-translational degree-of-freedom,
   EQ.4: x and y-translational degrees-of-freedom,
   EQ.5: y and z-translational degrees-of-freedom,
   EQ.6: z and x-translational degrees-of-freedom,
   EQ.7: x, y, and z-translational degrees-of-freedom.
   This option does not apply to the spot weld, beam type 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: rr1
   :type: int


   
   Get or set the Release conditions for rotations at node N1.
   EQ.0: no rotational degrees-of-freedom are released,
   EQ.1: x-rotational degree-of-freedom,
   EQ.2: y-rotational degree-of-freedom,
   EQ.3: z-rotational degree-of-freedom,
   EQ.4: x and y-rotational degrees-of-freedom,
   EQ.5: y and z-rotational degrees-of-freedom,
   EQ.6: z and x-rotational degrees-of-freedom,
   EQ.7: x, y, and z-rotational degrees-of-freedom.
   This option does not apply to the spot weld, beam type 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: rt2
   :type: int


   
   Get or set the Release conditions for translations at node N2.
   EQ.0: no translational degrees-of-freedom are released,
   EQ.1: x-translational degree-of-freedom,
   EQ.2: y-translational degree-of-freedom,
   EQ.3: z-translational degree-of-freedom,
   EQ.4: x and y-translational degrees-of-freedom,
   EQ.5: y and z-translational degrees-of-freedom,
   EQ.6: z and x-translational degrees-of-freedom,
   EQ.7: x, y, and z-translational degrees-of-freedom.
   This option does not apply to the spot weld, beam type 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: rr2
   :type: int


   
   Get or set the Release conditions for rotations at node N2.
   EQ.0: no rotational degrees-of-freedom are released,
   EQ.1: x-rotational degree-of-freedom,
   EQ.2: y-rotational degree-of-freedom,
   EQ.3: z-rotational degree-of-freedom,
   EQ.4: x and y-rotational degrees-of-freedom,
   EQ.5: y and z-rotational degrees-of-freedom,
   EQ.6: z and x-rotational degrees-of-freedom,
   EQ.7: x, y, and z-rotational degrees-of-freedom.
   This option does not apply to the spot weld, beam type 9.
















   ..
       !! processed by numpydoc !!

.. py:property:: local
   :type: int


   
   Get or set the Coordinate system
   EQ.1-global system
   EQ.2-Local system (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: str


   
   Get or set the Section type (A format):
   EQ.SECTION_01: I-shape          EQ.SECTION_12: Cross
   EQ.SECTION_02: Channel  EQ.SECTION_13: H-shape
   EQ.SECTION_03: L-shape          EQ.SECTION_14: T-shape2
   EQ.SECTION_04: T-shape          EQ.SECTION_15: I-shape3
   EQ.SECTION_05: Tubular box      EQ.SECTION_16: Channel2
   EQ.SECTION_06: Z-shape          EQ.SECTION_17: Channel3
   EQ.SECTION_07: Trapezoidal      EQ.SECTION_18: T-shape3
   EQ.SECTION_08: Circular         EQ.SECTION_19: Box-shape2
   EQ.SECTION_09: Tubular          EQ.SECTION_20: Hexagon
   EQ.SECTION_10: I-shape2 EQ.SECTION_21: Hat-shape
   EQ.SECTION_11: Solid box        EQ.SECTION_22: Hat-shape2
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Input parameters for section option using STYPE above
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Input parameters for section option using STYPE above
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Input parameters for section option using STYPE above
















   ..
       !! processed by numpydoc !!

.. py:property:: d4
   :type: Optional[float]


   
   Get or set the Input parameters for section option using STYPE above
















   ..
       !! processed by numpydoc !!

.. py:property:: d5
   :type: Optional[float]


   
   Get or set the Input parameters for section option using STYPE above
















   ..
       !! processed by numpydoc !!

.. py:property:: d6
   :type: Optional[float]


   
   Get or set the Input parameters for section option using STYPE above
















   ..
       !! processed by numpydoc !!

.. py:property:: wx1
   :type: float


   
   Get or set the Offset vector at modal point N1.
















   ..
       !! processed by numpydoc !!

.. py:property:: wy1
   :type: float


   
   Get or set the Offset vector at modal point N1.
















   ..
       !! processed by numpydoc !!

.. py:property:: wz1
   :type: float


   
   Get or set the Offset vector at modal point N1.
















   ..
       !! processed by numpydoc !!

.. py:property:: wx2
   :type: float


   
   Get or set the Offset vector at modal point N2.
















   ..
       !! processed by numpydoc !!

.. py:property:: wy2
   :type: float


   
   Get or set the Offset vector at modal point N2.
















   ..
       !! processed by numpydoc !!

.. py:property:: wz2
   :type: float


   
   Get or set the Offset vector at modal point N2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'BEAM_SECTION_OFFSET'






