





:class:`ControlShell`
=====================


.. py:class:: control_shell.ControlShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~wrpang`
            - Get or set the Shell element warpage angle in degrees. If a warpage greater than this angle is found, a warning message is printed. (Default is 20 degrees).
          * - :py:attr:`~esort`
            - Get or set the Automatic sorting of triangular shell elements to treat degenerate quadrilateral shell elements as C0 triangular shells, (see option THEORY inuser's manual):
          * - :py:attr:`~irnxx`
            - Get or set the Shell normal update option.  This option affects the Hughes-Liu, Belytschko-Wong-Chiang, and the Belytschko-Tsay shell formultions. The latter is affected if and only if the warping stiffness option is active, i.e. BWC=1. IRNXX must be set to 2 to invoke the top or bottom surface as the reference surface for the Hughes-Liu shell elements.
          * - :py:attr:`~istupd`
            - Get or set the Shell thickness change option for deformable shells. The parameter, PSSTUPD, on the second optional card allows this option to be applied by part ID. For crash analysis, neglecting the elastic component of the strains, ISTUPD=4, may improve enery conservation and stability.
          * - :py:attr:`~theory`
            - Get or set the Default shell theory:
          * - :py:attr:`~bwc`
            - Get or set the Warping stiffness for Belytschko-Tsay shells:
          * - :py:attr:`~miter`
            - Get or set the Plane stress plasticity option (applies to materials 3, 18, 19, and 24):
          * - :py:attr:`~proj`
            - Get or set the Projection method for warping stiffness in the Belytschko-Tsay shell and Belytschko-Wong-Chiang elements (See Remarks in user's manual).
          * - :py:attr:`~rotascl`
            - Get or set the Define a scale factor for the rotary shell mass. This option is not for general use. The rotary inertia for shells is automatically scaled to permit a larger time step size. A scale factor other than the default, i.e., unity, is not recommended.
          * - :py:attr:`~intgrd`
            - Get or set the Default shell through thickness numerical integration rule:
          * - :py:attr:`~lamsht`
            - Get or set the For composite shells with material types:
          * - :py:attr:`~cstyp6`
            - Get or set the Coordinate system for the type 6 shell element. The default system computes a unique local system at each inplane point. The uniform local system computes just one system used throughout the shell element. This involves fewer calculations and is therefore more efficient. The change of systems has a slight effect on results; therefore, the older method less efficient method is the default.
          * - :py:attr:`~thshel`
            - Get or set the Thermal shell option.  Four node shells are treated internally as twelve node brick elements to allow heat conduction through the thickness of the shell.
          * - :py:attr:`~psstupd`
            - Get or set the |PSSTUPD| is the optional shell part set ID specifying which part ID's have or do not have their thickness updated.  The shell thickness update by default applies to all shell elements in the mesh.  Generally, this part set ID is not needed.
          * - :py:attr:`~sidt4tu`
            - Get or set the Part set ID for parts which use the type 4 thickness update where elastic strains are ignored. This option is useful if different components of the final model are validated using different update options.
          * - :py:attr:`~cntco`
            - Get or set the Flag to account for shell reference surface offsets in the contact treatment
          * - :py:attr:`~itsflg`
            - Get or set the Flag to activate/deactivate initial transverse shear stresses:
          * - :py:attr:`~irquad`
            - Get or set the In plane integration rule for the 8 node shell element:
          * - :py:attr:`~w_mode`
            - Get or set the W-Mode amplitude for element deletion, specified in degrees
          * - :py:attr:`~stretch`
            - Get or set the Stretch ratio of element diagonals for element deletion. This option is activated if and only if either NFAIL1 or NFAIL4 are nonzero and STRETCH > 0.0
          * - :py:attr:`~icrq`
            - Get or set the Continuous treatment across element edges for some specified result quantities.
          * - :py:attr:`~nfail1`
            - Get or set the Flag to check for highly distorted under-integrated shell elements, print a
          * - :py:attr:`~nfail4`
            - Get or set the Flag to check for highly distorted fully-integrated shell elements, print a
          * - :py:attr:`~psnfail`
            - Get or set the Optional shell part set ID specifying which part IDs are checked by the FAIL1 and ¦ÒFAIL4 options. If zero, all shell part IDs are included
          * - :py:attr:`~keepcs`
            - Get or set the Flag to keep the contact segments of failed shell elements in the
          * - :py:attr:`~delfr`
            - Get or set the Flag to delete shell elements whose neighboring shell elements have failed; consequently, the shell is detached from the structure and moving freely in space.  This condition is checked if NFAIL1 or NFAIL4 are nonzero.
          * - :py:attr:`~drcpsid`
            - Get or set the Part set ID for drilling rotation constraint method.
          * - :py:attr:`~drcprm`
            - Get or set the Drilling rotation constraint parameter (default=1.0).
          * - :py:attr:`~intperr`
            - Get or set the Flag for behavior in case of unwanted interpolation/extrapolation of
          * - :py:attr:`~drcmth`
            - Get or set the Drilling rotation constraint method. Options to choose how drilling kinematics are determined.
          * - :py:attr:`~lispsid`
            - Get or set the Part set ID related to *INITIAL_STRESS_SHELL. For all parts in this set,
          * - :py:attr:`~nlocdt`
            - Get or set the Flag for time step handling for shell elements with offset. If the shell reference surface is offset by NLOC (*SECTION_SHELL) or OFFSET (*ELEMENT_SHELL), the time step size of those shell elements is reduced to fix instabilities. The reduction of the time step size is based on numerical tests which show a dependence on the offset distance and the ratio of shell thickness to edge length (T/L).


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

    from control_shell import ControlShell

Property detail
---------------

.. py:property:: wrpang
   :type: float


   
   Get or set the Shell element warpage angle in degrees. If a warpage greater than this angle is found, a warning message is printed. (Default is 20 degrees).
















   ..
       !! processed by numpydoc !!

.. py:property:: esort
   :type: int


   
   Get or set the Automatic sorting of triangular shell elements to treat degenerate quadrilateral shell elements as C0 triangular shells, (see option THEORY inuser's manual):
   EQ.0: no sorting required (default).
   EQ.1: full sorting (C0 triangular shells),
   EQ.2: full sorting (DKT triangular shells)
















   ..
       !! processed by numpydoc !!

.. py:property:: irnxx
   :type: int


   
   Get or set the Shell normal update option.  This option affects the Hughes-Liu, Belytschko-Wong-Chiang, and the Belytschko-Tsay shell formultions. The latter is affected if and only if the warping stiffness option is active, i.e. BWC=1. IRNXX must be set to 2 to invoke the top or bottom surface as the reference surface for the Hughes-Liu shell elements.
   EQ.-2: unique nodal fibers which are incrementally updated based on the nodal rotation at the location of the fiber,
   EQ.-1: recompute fiber directions each cycle,
   EQ.0: default set to -1,
   EQ.1: compute on restarts,
   EQ.n: compute every n cycles (Hughes-Liu shells only).
















   ..
       !! processed by numpydoc !!

.. py:property:: istupd
   :type: int


   
   Get or set the Shell thickness change option for deformable shells. The parameter, PSSTUPD, on the second optional card allows this option to be applied by part ID. For crash analysis, neglecting the elastic component of the strains, ISTUPD=4, may improve enery conservation and stability.
   EQ.0: no change.
   EQ.1: membrane straining causes thickness change (important for sheet metal forming or whenever membrane stretching is important).
   EQ.2: membrane straining causes thickness change in 8 node thick shell elements, types 1 and 2. This option is not recommended for implicit or explicit solutions which use the fully integrated type 2 element. The type 3 thick shell is a continuum based shell and thickness changes are always considered.
   EQ.3: options 1 and 2 apply.
   EQ.4: option 1 applies, but the elastic strains are neglected for the thickness update. This option only  applies to the most common elastic-plastic materials for which the elastic response is isotropic.
















   ..
       !! processed by numpydoc !!

.. py:property:: theory
   :type: int


   
   Get or set the Default shell theory:
   EQ.1: Hughes-Liu,
   EQ.2: Belytschko-Tsay (default),
   EQ.3: BCIZ triangular shell (not recommended),
   EQ.4: Co triangular shell,
   EQ.5: Belytschko-Tsay membrane,
   EQ.6: S/R Hughes Liu,
   EQ.7: S/R co-rotational Hughes Liu,
   EQ.8: Belytschko-Leviathan shell,
   EQ.9: fully integrated Belytschko-Tsay membrane,
   EQ.10: Belytschko-Wong-Chiang,
   EQ.11: Fast (co-rotational) Hughes-Liu.
   EQ.12: Plane stress (x-y plane),
   EQ.13: Plane strain (x-y plane),
   EQ.14: Axisymmetric solid (y-axis of symmetry) - area weighted,
   EQ.15: Axisymmetric solid (y-axis of symmetry) - volume weighted
   EQ.16: Fully integrated shell element (very fast)
   EQ.17: Discrete Kirchhoff triangular shell (DKT)
   EQ.18: Discrete Kirchhoff linear shell either quadrilateral or triangular
   EQ.20: C0 linear shell element with drilling stiffness.
   For the 2D axisymmetric solid elements, high explosive applications work best with the area weighted approach and structural applications work best with the volume weighted approach. The volume weighted approach can lead to problems along the axis of symmetry under very large deformations.  Often the symmetry condition is not obeyed, and the elements will kink along the axis. The volume weigthed approach must be used if 2D shell elements are used in the mesh. Type 14 and 15 elements cannot be mixed in the same calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: bwc
   :type: int


   
   Get or set the Warping stiffness for Belytschko-Tsay shells:
   EQ.1: Belytschko-Wong-Chiang warping stiffness added.
   EQ.2: Belytschko-Tsay (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: miter
   :type: int


   
   Get or set the Plane stress plasticity option (applies to materials 3, 18, 19, and 24):
   EQ.1: iterative plasticity with 3 secant iterations (default),
   EQ.2: full iterative plasticity,
   EQ.3: radial return noniterative plasticity. May lead to false results and has to be used with great care.
















   ..
       !! processed by numpydoc !!

.. py:property:: proj
   :type: int


   
   Get or set the Projection method for warping stiffness in the Belytschko-Tsay shell and Belytschko-Wong-Chiang elements (See Remarks in user's manual).
   EQ.0: drill projection,
   EQ.1: full projection.
















   ..
       !! processed by numpydoc !!

.. py:property:: rotascl
   :type: float


   
   Get or set the Define a scale factor for the rotary shell mass. This option is not for general use. The rotary inertia for shells is automatically scaled to permit a larger time step size. A scale factor other than the default, i.e., unity, is not recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: intgrd
   :type: int


   
   Get or set the Default shell through thickness numerical integration rule:
   EQ.0: Gauss integration. If 1-10 integration points are specified, the default rule is Gauss integration.
   EQ.1: Lobatto integration. If 3-10 integration points are specified, the default rule is Lobatto. For 2 point integration, the Lobatto rule is very inaccurate, so Gauss integration is used instead. Lobatto integration has an advantage in that the inner and outer integration points are on the shell surfaces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lamsht
   :type: int


   
   Get or set the For composite shells with material types:
   *MAT_COMPOSITE_DAMAGE
   *MAT_ENHANCED_COMPOSITE_DAMAGE.
   If this flag is set laminated shell theory is used. Lamination theory is applied to correct for the assumption of a uniform constant shear strain through the thickness of the shell. Unless this correction is applied, the stiffness of the shell can be grossly incorrect if there are drastic differences in the elastic constants from ply to ply, especially for sandwich type shells. Generally, without this correction the results are too stiff. For the discrete Kirchhoff shell elements, which do not consider transverse shear, this option is ignored.
   EQ.0: do not update shear corrections,
   EQ.1: activate laminated shell theory.
















   ..
       !! processed by numpydoc !!

.. py:property:: cstyp6
   :type: int


   
   Get or set the Coordinate system for the type 6 shell element. The default system computes a unique local system at each inplane point. The uniform local system computes just one system used throughout the shell element. This involves fewer calculations and is therefore more efficient. The change of systems has a slight effect on results; therefore, the older method less efficient method is the default.
   EQ.1:  variable local coordinate system  (default),
   EQ.2:  uniform local system.
















   ..
       !! processed by numpydoc !!

.. py:property:: thshel
   :type: int


   
   Get or set the Thermal shell option.  Four node shells are treated internally as twelve node brick elements to allow heat conduction through the thickness of the shell.
















   ..
       !! processed by numpydoc !!

.. py:property:: psstupd
   :type: int


   
   Get or set the |PSSTUPD| is the optional shell part set ID specifying which part ID's have or do not have their thickness updated.  The shell thickness update by default applies to all shell elements in the mesh.  Generally, this part set ID is not needed.
   LT.0: these shell parts are excluded from the shell thickness update
   EQ.0: all deformable shells have their thickness updated
   GT.0: these shell parts are included in the shell thickness update
















   ..
       !! processed by numpydoc !!

.. py:property:: sidt4tu
   :type: int


   
   Get or set the Part set ID for parts which use the type 4 thickness update where elastic strains are ignored. This option is useful if different components of the final model are validated using different update options.
















   ..
       !! processed by numpydoc !!

.. py:property:: cntco
   :type: int


   
   Get or set the Flag to account for shell reference surface offsets in the contact treatment
   EQ.0: offsets are ignored
   EQ.1: offsets are treated using shell thickness
   EQ.2: offsets are treated using the user defined contact thickness which may be different than the shell thickness used in the element formulations
















   ..
       !! processed by numpydoc !!

.. py:property:: itsflg
   :type: int


   
   Get or set the Flag to activate/deactivate initial transverse shear stresses:
   EQ.0: keep transverse shear stresses
   EQ.1: set transverse shear stresses to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: irquad
   :type: int


   
   Get or set the In plane integration rule for the 8 node shell element:
   EQ.2: 2 x 2 Gauss quadrature,
   EQ.3: 3 x 3 Gauss quadrature..
















   ..
       !! processed by numpydoc !!

.. py:property:: w_mode
   :type: Optional[float]


   
   Get or set the W-Mode amplitude for element deletion, specified in degrees
















   ..
       !! processed by numpydoc !!

.. py:property:: stretch
   :type: Optional[float]


   
   Get or set the Stretch ratio of element diagonals for element deletion. This option is activated if and only if either NFAIL1 or NFAIL4 are nonzero and STRETCH > 0.0
















   ..
       !! processed by numpydoc !!

.. py:property:: icrq
   :type: int


   
   Get or set the Continuous treatment across element edges for some specified result quantities.
   EQ.0: not active
   EQ.1: thickness and plastic strain
















   ..
       !! processed by numpydoc !!

.. py:property:: nfail1
   :type: Optional[int]


   
   Get or set the Flag to check for highly distorted under-integrated shell elements, print a
   message, and delete the element or terminate. Generally, this flag is not
   needed for one point elements that do not use the warping stiffness. A
   distorted element is one where a negative Jacobian exist within the
   domain of the shell, not just at integration points. The checks are made
   away from the CPU requirements for one point elements. If nonzero,
   NFAIL1 can be changed in a restart.
   EQ.1: print message and delete element.
   EQ.2: print message, write D3DUMP file, and terminate
   GT.2: print message and delete element. When NFAIL1 elements
   are deleted then write D3DUMP file and terminate. These NFAIL1
   failed elements also include all shell elements that failed for other
   reasons than distortion. Before the D3DUMP file is written, NFAIL1
   is doubled, so the run can immediately be continued if desired.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfail4
   :type: Optional[int]


   
   Get or set the Flag to check for highly distorted fully-integrated shell elements, print a
   message and delete the element or terminate. Generally, this flag is
   recommended. A distorted element is one where a negative Jacobian
   exist within the domain of the shell, not just at integration points. The
   checks are made away from the integration points to enable the bad
   elements to be deleted before an instability leading to an error
   termination occurs. If nonzero, NFAIL1 can be changed in a restart.
   EQ.1: print message and delete element.
   EQ.2: print message, write D3DUMP file, and terminate
   GT.2: print message and delete element. When NFAIL4 elements
   are deleted then write D3DUMP file and terminate. These NFAIL4
   failed elements also include all shell elements that failed for other
   reasons than distortion. Before the D3DUMP file is written,
   NFAIL4 is doubled, so the run can immediately be continued if
   desired.
















   ..
       !! processed by numpydoc !!

.. py:property:: psnfail
   :type: int


   
   Get or set the Optional shell part set ID specifying which part IDs are checked by the FAIL1 and ¦ÒFAIL4 options. If zero, all shell part IDs are included
















   ..
       !! processed by numpydoc !!

.. py:property:: keepcs
   :type: int


   
   Get or set the Flag to keep the contact segments of failed shell elements in the
   calculation. The contact segments of the failed shells remain active
   until a node shared by the segments has no active shells attached. Only
   then are the segments deleted..
   EQ.0: Inactive
   EQ.1: Active.
















   ..
       !! processed by numpydoc !!

.. py:property:: delfr
   :type: int


   
   Get or set the Flag to delete shell elements whose neighboring shell elements have failed; consequently, the shell is detached from the structure and moving freely in space.  This condition is checked if NFAIL1 or NFAIL4 are nonzero.
   EQ.0:   Inactive
   EQ.1:   Isolated elements are deleted.
   EQ.2:   QuadrilateralIsolated quadrilateral elements that are isolated and triangular elements that are connected by only one node are deleted.
   EQ.3:   Elements that are either isolated or connected by only one node are deleted.
















   ..
       !! processed by numpydoc !!

.. py:property:: drcpsid
   :type: int


   
   Get or set the Part set ID for drilling rotation constraint method.
















   ..
       !! processed by numpydoc !!

.. py:property:: drcprm
   :type: float


   
   Get or set the Drilling rotation constraint parameter (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: intperr
   :type: int


   
   Get or set the Flag for behavior in case of unwanted interpolation/extrapolation of
   initial stresses from *INITIAL_STRESS_SHELL.
   EQ.0: Only warning is written, calculation continues (default).
   EQ.1: Error exit, calculation stops.
















   ..
       !! processed by numpydoc !!

.. py:property:: drcmth
   :type: int


   
   Get or set the Drilling rotation constraint method. Options to choose how drilling kinematics are determined.
   EQ.0:   Generalized drilling strain rate at shell element nodes involving drill rotation at the specific node plus
   the translational velocities of two adjacent nodes.See more details in Erhart and Borrvall[2013].
   EQ.1 : Direct use of the spin tensor(e.g.see section 21 in the LS - DYNA Theory Manual) with respect to the shell
   element normal direction, numerically integrated at element level.A similar approach is described in Kanok - Nukulchai[1979].
















   ..
       !! processed by numpydoc !!

.. py:property:: lispsid
   :type: int


   
   Get or set the Part set ID related to *INITIAL_STRESS_SHELL. For all parts in this set,
   the initial stress components SIGXX, SIGYY, ..., SIGZX are defined in the local (element) coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlocdt
   :type: int


   
   Get or set the Flag for time step handling for shell elements with offset. If the shell reference surface is offset by NLOC (*SECTION_SHELL) or OFFSET (*ELEMENT_SHELL), the time step size of those shell elements is reduced to fix instabilities. The reduction of the time step size is based on numerical tests which show a dependence on the offset distance and the ratio of shell thickness to edge length (T/L).
   EQ.0:   Reduce time step size up to 10 % to avoid instabilities.Care has to be taken since a smaller time step will lead to larger masses due to mass scaling.
   EQ.1 : No reduction of time step to restore prior behavior if necessary.Instabilities were most likely observed for aspect ratios of T / L > 0.5
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SHELL'






