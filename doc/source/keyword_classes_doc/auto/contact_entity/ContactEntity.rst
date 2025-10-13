





:class:`ContactEntity`
======================


.. py:class:: contact_entity.ContactEntity(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_ENTITY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactEntity

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the rigid body to which the geometric entity is attached, see *PART.
          * - :py:attr:`~geotyp`
            - Get or set the Type of geometric entity:
          * - :py:attr:`~ssid`
            - Get or set the Slave set ID, see *SET_NODE_OPTION, *PART, or *SET_PART.
          * - :py:attr:`~sstyp`
            - Get or set the Slave set type:
          * - :py:attr:`~sf`
            - Get or set the Penalty scale factor. Useful to scale maximized penalty.
          * - :py:attr:`~df`
            - Get or set the Damping option, see description for *CONTACT_OPTION:
          * - :py:attr:`~cf`
            - Get or set the Coulomb friction coefficient. Assumed to be constant (default=0.0).
          * - :py:attr:`~intord`
            - Get or set the Integration order (slaved materials only). This option is not available with entity types 8 and 9 where only nodes are checked:
          * - :py:attr:`~bt`
            - Get or set the Birth time (default=0.0).
          * - :py:attr:`~dt`
            - Get or set the Death time (default=1.0E+20).
          * - :py:attr:`~so`
            - Get or set the Flag to use penalty stiffness as in surface to surface contact:
          * - :py:attr:`~go`
            - Get or set the Flag for mesh generation of the contact entity for entity types 1-5 and 10-11. This is used for visualization in post-processing only:
          * - :py:attr:`~ithk`
            - Get or set the Flag for considering thickness for shell slave nodes (applies only
          * - :py:attr:`~spr`
            - Get or set the Include the slave side in *DATABASE_BINARY_INTFOR
          * - :py:attr:`~xc`
            - Get or set the xc, x-center.
          * - :py:attr:`~yc`
            - Get or set the yc, y-center.
          * - :py:attr:`~zc`
            - Get or set the zc, z-center.
          * - :py:attr:`~ax`
            - Get or set the Ax, x-direction for local axis A.
          * - :py:attr:`~ay`
            - Get or set the Ay, y-direction for local axis A.
          * - :py:attr:`~az`
            - Get or set the Az, z-direction for local axis A.
          * - :py:attr:`~bx`
            - Get or set the Bx, x-direction for local axis B.
          * - :py:attr:`~by`
            - Get or set the By, y-direction for local axis B.
          * - :py:attr:`~bz`
            - Get or set the Bz, z-direction for local axis B.
          * - :py:attr:`~inout`
            - Get or set the In-out flag. Allows contact from the inside or the outside of the entity:
          * - :py:attr:`~g1`
            - Get or set the Entity coefficient g1 (CAL3D/MADYMO plane or ellipse number) for coupled analysis.
          * - :py:attr:`~g2`
            - Get or set the Entity coefficient g2.
          * - :py:attr:`~g3`
            - Get or set the Entity coefficient g3.
          * - :py:attr:`~g4`
            - Get or set the Entity coefficient g4.
          * - :py:attr:`~g5`
            - Get or set the Entity coefficient g5.
          * - :py:attr:`~g6`
            - Get or set the Entity coefficient g6.
          * - :py:attr:`~g7`
            - Get or set the Entity coefficient g7.


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

    from contact_entity import ContactEntity

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the rigid body to which the geometric entity is attached, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: geotyp
   :type: int


   
   Get or set the Type of geometric entity:
   EQ.1: plane (default),
   EQ.2: sphere,
   EQ.3: cylinder,
   EQ.4: ellipsoid,
   EQ.5: torus,
   EQ.6: CAL3D/MADYMO plane, see Appendix F of USER MANUAL,
   EQ.7: CAL3D/MADYMO ellipsoid, see Appendix F of USER MANUAL,
   EQ.8: VDA surface, see Appendix I of USER MANUAL,
   EQ.9: rigid body finite element mesh (shells only),
   EQ.10: finite plane,
   EQ.11: load curve defining line as surface profile of axisymmetric rigid bodies.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Slave set ID, see *SET_NODE_OPTION, *PART, or *SET_PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: sstyp
   :type: int


   
   Get or set the Slave set type:
   EQ.0: node set (default),
   EQ.1: part ID,
   EQ.2: part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Penalty scale factor. Useful to scale maximized penalty.
















   ..
       !! processed by numpydoc !!

.. py:property:: df
   :type: float


   
   Get or set the Damping option, see description for *CONTACT_OPTION:
   EQ.0.0: no damping (default),
   GT.0.0: viscous damping in percent of critical, e.g., 20.0 for 20% damping,
   EQ.-n: |n| is the load curve ID giving the damping force versus relative normal velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: cf
   :type: float


   
   Get or set the Coulomb friction coefficient. Assumed to be constant (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: intord
   :type: int


   
   Get or set the Integration order (slaved materials only). This option is not available with entity types 8 and 9 where only nodes are checked:
   EQ.0: check nodes only (default),
   EQ.1: 1-point integration over segments,
   EQ.2: 2x2 integration,
   EQ.3: 3x3 integration,
   EQ.4: 4x4 integration,
   EQ.5: 5x5 integration.
   This option allows a check of the penetration of the rigid body into the deformable (slaved) material. Then virtual nodes at the location of the integration points are checked.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time (default=1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: so
   :type: int


   
   Get or set the Flag to use penalty stiffness as in surface to surface contact:
   EQ.0: contact entity stiffness formulation (default),
   EQ.1: surface to surface contact method,
   EQ.-n: |n| is the load curve ID giving the force versus the normal penetration.
















   ..
       !! processed by numpydoc !!

.. py:property:: go
   :type: int


   
   Get or set the Flag for mesh generation of the contact entity for entity types 1-5 and 10-11. This is used for visualization in post-processing only:
   EQ.0: mesh is not generated (default),
   EQ.1: mesh is generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: ithk
   :type: int


   
   Get or set the Flag for considering thickness for shell slave nodes (applies only
   to entity types 1, 2, 3; SSTYP must be set to zero).
   EQ.0: shell thickness is not considered,
   EQ.1: shell thickness is considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: spr
   :type: int


   
   Get or set the Include the slave side in *DATABASE_BINARY_INTFOR
   interface force files; valid only when SSTYP > 0:
   EQ.1: slave side forces included..
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the xc, x-center.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the yc, y-center.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the zc, z-center.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: ax
   :type: float


   
   Get or set the Ax, x-direction for local axis A.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: ay
   :type: float


   
   Get or set the Ay, y-direction for local axis A.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: az
   :type: float


   
   Get or set the Az, z-direction for local axis A.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: bx
   :type: float


   
   Get or set the Bx, x-direction for local axis B.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: by
   :type: float


   
   Get or set the By, y-direction for local axis B.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: bz
   :type: float


   
   Get or set the Bz, z-direction for local axis B.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: inout
   :type: int


   
   Get or set the In-out flag. Allows contact from the inside or the outside of the entity:
   EQ.0: slave nodes exist outside of the entity (default),
   EQ.1: slave nodes exist inside the entity.
















   ..
       !! processed by numpydoc !!

.. py:property:: g1
   :type: float


   
   Get or set the Entity coefficient g1 (CAL3D/MADYMO plane or ellipse number) for coupled analysis.
   For further information please see USER MANUAL section 6.33 and Appendix F.
















   ..
       !! processed by numpydoc !!

.. py:property:: g2
   :type: float


   
   Get or set the Entity coefficient g2.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: g3
   :type: float


   
   Get or set the Entity coefficient g3.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: g4
   :type: float


   
   Get or set the Entity coefficient g4.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: g5
   :type: float


   
   Get or set the Entity coefficient g5.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: g6
   :type: float


   
   Get or set the Entity coefficient g6.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!

.. py:property:: g7
   :type: float


   
   Get or set the Entity coefficient g7.
   For further information please see USER MANUAL section 6.33.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'ENTITY'






