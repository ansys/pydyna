





:class:`ElementBeamThicknessScalar`
===================================


.. py:class:: element_beam_thickness_scalar.ElementBeamThicknessScalar(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_BEAM_THICKNESS_SCALAR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementBeamThicknessScalar

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
          * - :py:attr:`~vol`
            - Get or set the Volume of discrete beam and scalar beam. If the mass density of the material model for the discrete beam is set to unity, the magnitude of the lumped mass can be defined here instead. This lumped mass is partitioned to the two nodes of the beam element. The translational time step size for the type 6 beam is dependent on the volume, mass density, and the translational stiffness values, so it is important to define this parameter. Defining the volume is also essential for mass scaling if the type 6 beam controls the time step size.
          * - :py:attr:`~iner`
            - Get or set the Mass moment of inertia for the six degree of freedom discrete beam and scalar beam. This lumped inertia is partitioned to the two nodes of the beam element. The rotational time step size for the type 6 beam is dependent on the lumped inertia and the rotational stiffness values, so it is important to define this parameter if the rotational springs are active. Defining the rotational inertia is also essential for mass scaling if the type 6 beam rotational stiffness controls the time step size.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for orientation, materials type ID (66-69, 93, 95, 97, 121, 146), see *DEFINE_COORDINATE_SYSTEM. If CID=0, a default coordinate system is defined in the global system or on the third node of the beam, which is used for orientation. This option is not defined for material types than act between two nodal points, such as cable elements. The coordinate system rotates with the discrete beam, see SCOOR above.
          * - :py:attr:`~dofn1`
            - Get or set the Active degree-of-freedom at node 1, a number between 1 to 6 where 1 in x-translation and 4 is x-rotation.
          * - :py:attr:`~dofn2`
            - Get or set the Active degree-of-freedom at node 2, a number between 1 to 6.


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

    from element_beam_thickness_scalar import ElementBeamThicknessScalar

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

.. py:property:: vol
   :type: Optional[float]


   
   Get or set the Volume of discrete beam and scalar beam. If the mass density of the material model for the discrete beam is set to unity, the magnitude of the lumped mass can be defined here instead. This lumped mass is partitioned to the two nodes of the beam element. The translational time step size for the type 6 beam is dependent on the volume, mass density, and the translational stiffness values, so it is important to define this parameter. Defining the volume is also essential for mass scaling if the type 6 beam controls the time step size.
















   ..
       !! processed by numpydoc !!

.. py:property:: iner
   :type: Optional[float]


   
   Get or set the Mass moment of inertia for the six degree of freedom discrete beam and scalar beam. This lumped inertia is partitioned to the two nodes of the beam element. The rotational time step size for the type 6 beam is dependent on the lumped inertia and the rotational stiffness values, so it is important to define this parameter if the rotational springs are active. Defining the rotational inertia is also essential for mass scaling if the type 6 beam rotational stiffness controls the time step size.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for orientation, materials type ID (66-69, 93, 95, 97, 121, 146), see *DEFINE_COORDINATE_SYSTEM. If CID=0, a default coordinate system is defined in the global system or on the third node of the beam, which is used for orientation. This option is not defined for material types than act between two nodal points, such as cable elements. The coordinate system rotates with the discrete beam, see SCOOR above.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofn1
   :type: float


   
   Get or set the Active degree-of-freedom at node 1, a number between 1 to 6 where 1 in x-translation and 4 is x-rotation.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofn2
   :type: float


   
   Get or set the Active degree-of-freedom at node 2, a number between 1 to 6.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'BEAM_THICKNESS_SCALAR'






