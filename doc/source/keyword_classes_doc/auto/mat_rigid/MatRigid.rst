





:class:`MatRigid`
=================


.. py:class:: mat_rigid.MatRigid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_RIGID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatRigid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus. Reasonable values have to be chosen for contact analysis (choice of penalty).
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio. Reasonable values have to be chosen for contact analysis (choice of penalty).
          * - :py:attr:`~n`
            - Get or set the MADYMO3D (not CAL3D) coupling flag, n:
          * - :py:attr:`~couple`
            - Get or set the Coupling option if applicable:
          * - :py:attr:`~m`
            - Get or set the MADYMO/CAL3D Coupling option flag:
          * - :py:attr:`~alias`
            - Get or set the VDA surface alias name, see keyword manual Appendix I.
          * - :py:attr:`~cmo`
            - Get or set the Center of mass constraint option, CMO:
          * - :py:attr:`~con1`
            - Get or set the Global translational constraint:
          * - :py:attr:`~con2`
            - Get or set the Global rotational constraint:
          * - :py:attr:`~lco_or_a1`
            - Get or set the EQ.LCO: Local coordinate system number for output. See *DEFINE_COORDINATE,
          * - :py:attr:`~a2`
            - Get or set the Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
          * - :py:attr:`~v1`
            - Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from mat_rigid import MatRigid

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus. Reasonable values have to be chosen for contact analysis (choice of penalty).
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio. Reasonable values have to be chosen for contact analysis (choice of penalty).
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: float


   
   Get or set the MADYMO3D (not CAL3D) coupling flag, n:
   EQ.0: use normal LS-DYNA rigid body updates,
   GT.0: the rigid body is coupled to MADYMO ellipsoid number n,
   LT.0: the rigid body is coupled to MADYMO plane number |n|.
















   ..
       !! processed by numpydoc !!

.. py:property:: couple
   :type: float


   
   Get or set the Coupling option if applicable:
   EQ.-1: attach VDA surface in ALIAS (defined in the eighth field) and automatically generate a mesh for viewing the surface in LS-TAURUS.

   MADYMO3D/CAL3D coupling option:
   EQ.0: the undeformed geometry input to LS-DYNA corresponds to the local system for MADYMO/CAL3D. The finite element mesh is input,
   EQ.1: the undeformed geometry input to LS-DYNA corresponds to the global system for MADYMO/CAL3D,
   EQ.2: generate a mesh for the ellipsoids and planes internally in LS-DYNA3D.















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: float


   
   Get or set the MADYMO/CAL3D Coupling option flag:
   EQ.0: use normal LS-DYNA rigid body updates,
   EQ.m: this rigid body corresponds to MADYMO/CAL3D system number m. Rigid body updates are performed by MADYMO/CAL3D.
















   ..
       !! processed by numpydoc !!

.. py:property:: alias
   :type: Optional[str]


   
   Get or set the VDA surface alias name, see keyword manual Appendix I.
















   ..
       !! processed by numpydoc !!

.. py:property:: cmo
   :type: float


   
   Get or set the Center of mass constraint option, CMO:
   EQ.+1: constraints applied in global directions,
   EQ.0: no constraints
   EQ.-1: constraints applied in local directions (SPC constraint).
















   ..
       !! processed by numpydoc !!

.. py:property:: con1
   :type: Optional[float]


   
   Get or set the Global translational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x displacement,
   EQ.2: constrained y displacement,
   EQ.3: constrained z displacement,
   EQ.4: constrained x and y displacements,
   EQ.5: constrained y and z displacements,
   EQ.6: constrained z and x displacements,
   EQ.7: constrained x, y, and z displacements.

   If CM0=-1.0:
   Define local coordinate system ID. See *DEFINE_ COORDINATE_OPTION: This coordinate system is fixed in time.















   ..
       !! processed by numpydoc !!

.. py:property:: con2
   :type: Optional[float]


   
   Get or set the Global rotational constraint:
   EQ.0: no constraints,
   EQ.1: constrained x rotation,
   EQ.2: constrained y rotation,
   EQ.3: constrained z rotation,
   EQ.4: constrained x and y rotations,
   EQ.5: constrained y and z rotations,
   EQ.6: constrained z and x rotations,
   EQ.7: constrained x, y, and z rotations.

   If CM0=-1.0:
   EQ.000000 no constraint,
   EQ.100000 constrained x translation,
   EQ.010000 constrained y translation,
   EQ.001000 constrained z translation,
   EQ.000100 constrained x rotation,
   EQ.000010 constrained y rotation,
   EQ.000001 constrained z rotation.















   ..
       !! processed by numpydoc !!

.. py:property:: lco_or_a1
   :type: Optional[float]


   
   Get or set the EQ.LCO: Local coordinate system number for output. See *DEFINE_COORDINATE,
   EQ.A1: Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Component of vector a which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v which is fixed in the rigid body which are used for output and the user defined airbag sensor subroutines.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'RIGID'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





