





:class:`ElementBeamThicknessOrientation`
========================================


.. py:class:: element_beam_thickness_orientation.ElementBeamThicknessOrientation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_BEAM_THICKNESS_ORIENTATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementBeamThicknessOrientation

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
          * - :py:attr:`~parm1`
            - Get or set the Based on beam type:
          * - :py:attr:`~parm2`
            - Get or set the Based on beam type:
          * - :py:attr:`~parm3`
            - Get or set the Based on beam type:
          * - :py:attr:`~parm4`
            - Get or set the Based on beam type:
          * - :py:attr:`~parm5`
            - Get or set the Based on beam type:
          * - :py:attr:`~vx`
            - Get or set the Coordinates of an orientation vector relative to node N1. In this
          * - :py:attr:`~vy`
            - Get or set the Coordinates of an orientation vector relative to node N1. In this
          * - :py:attr:`~vz`
            - Get or set the Coordinates of an orientation vector relative to node N1. In this


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

    from element_beam_thickness_orientation import ElementBeamThicknessOrientation

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

.. py:property:: parm1
   :type: Optional[float]


   
   Get or set the Based on beam type:
   Type.EQ.1: beam thickness, s direction at node 1
   Type.EQ.2: area
   Type.EQ.3: area
   Type.EQ.4: beam thickness, s direction at node 1
   Type.EQ.5: beam thickness, s direction at node 1
   Type.EQ.6: volume
   Type.EQ.7: beam thickness, s direction at node 1
   Type.EQ.8: beam thickness, s direction at node 1
   Type.EQ.9:beam thickness, s direction at node 1
















   ..
       !! processed by numpydoc !!

.. py:property:: parm2
   :type: Optional[float]


   
   Get or set the Based on beam type:
   Type.EQ.1: beam thickness, s direction at node 2
   Type.EQ.2: Iss
   Type.EQ.3: not used
   Type.EQ.4: beam thickness, s direction at node 2
   Type.EQ.5: beam thickness, s direction at node 2
   Type.EQ.6: geometric inertia
   Type.EQ.6: volume
   Type.EQ.7: beam thickness, s direction at node 2
   Type.EQ.8: beam thickness, s direction at node 2
   Type.EQ.9: beam thickness, s direction at node 2
















   ..
       !! processed by numpydoc !!

.. py:property:: parm3
   :type: Optional[float]


   
   Get or set the Based on beam type:
   Type.EQ.1: beam thickness, t direction at node 1
   Type.EQ.2: Itt
   Type.EQ.3: not used
   Type.EQ.4: beam thickness, t direction at node 1
   Type.EQ.5: beam thickness, t direction at node 1
   Type.EQ.6: local coordinate ID
   Type.EQ.7: not used.
   Type.EQ.8: not used.
   Type.EQ.9: beam thickness, t direction at node 1
















   ..
       !! processed by numpydoc !!

.. py:property:: parm4
   :type: Optional[float]


   
   Get or set the Based on beam type:
   Type.EQ.1: beam thickness, t direction at node 2
   Type.EQ.2: Irr
   Type.EQ.3: not used
   Type.EQ.4: beam thickness, t direction at node 2
   Type.EQ.5: beam thickness, t direction at node 2
   Type.EQ.6: area
   Type.EQ.7: not used.
   Type.EQ.8: not used.
   Type.EQ.9: beam thickness, t direction at node 2
















   ..
       !! processed by numpydoc !!

.. py:property:: parm5
   :type: Optional[float]


   
   Get or set the Based on beam type:
   Type.EQ.1: not used
   Type.EQ.2: shear area
   Type.EQ.3: not used
   Type.EQ.4: not used
   Type.EQ.5: not used
   Type.EQ.6: offset
   Type.EQ.7: not used
   Type.EQ.8: not used
   Type.EQ.9: not used
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Coordinates of an orientation vector relative to node N1. In this
   case, the orientation vector points to a virtual third node, so the
   field N3 should be left undefined.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Coordinates of an orientation vector relative to node N1. In this
   case, the orientation vector points to a virtual third node, so the
   field N3 should be left undefined.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Coordinates of an orientation vector relative to node N1. In this
   case, the orientation vector points to a virtual third node, so the
   field N3 should be left undefined.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'BEAM_THICKNESS_ORIENTATION'






