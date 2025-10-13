





:class:`ControlSph`
===================


.. py:class:: control_sph.ControlSph(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_SPH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlSph

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ncbs`
            - Get or set the Number of time steps between particle sorting.
          * - :py:attr:`~boxid`
            - Get or set the SPH approximations are computed inside a specified box(see DEFINE_BOX). When a particle has gone outside the BOX, it is deactivated. This will save computational time by eliminating particles that no longer interact with the structure.
          * - :py:attr:`~dt`
            - Get or set the Death time. Determines when the SPH calculations are stopped.
          * - :py:attr:`~idim`
            - Get or set the Space dimension for SPH particles:
          * - :py:attr:`~nmneigh`
            - Get or set the Defines the initial number of neighbors per particle. This variable is just for memory allocation of arrays during the initialization phase. During the calculation, some particles can request more neighbors and LS-DYNA will automatically adapt the size of that variable. Default value should apply for most applications.
          * - :py:attr:`~form`
            - Get or set the Particle approximation theory (Remark 2):
          * - :py:attr:`~start`
            - Get or set the Start time for particle approximation. Particle approximations will be computed when time of the analysis has reached the value defined in START.
          * - :py:attr:`~maxv`
            - Get or set the Maximum value for velocity for the SPH particles. Particles with a velocity greater than MAXV are deactivated.
          * - :py:attr:`~cont`
            - Get or set the Defines the computation of the particle approximation between two different SPH parts:
          * - :py:attr:`~deriv`
            - Get or set the Time integration type for the smoothing length. Please refer to 7.99 (CONTROL) on v970 LS-DYNA manual for EQ.0 and EQ.1.
          * - :py:attr:`~ini`
            - Get or set the Computation of the smoothing length during the initialization:
          * - :py:attr:`~ishow`
            - Get or set the Display options for SPH particles:
          * - :py:attr:`~ierod`
            - Get or set the Deactivation control for SPH particles:
          * - :py:attr:`~icont`
            - Get or set the Controls contact behavior for deactivated SPH particles:
          * - :py:attr:`~iavis`
            - Get or set the Defines artificial viscosity formulation for SPH elements (Remark 3):
          * - :py:attr:`~isymp`
            - Get or set the Defines the percentage of original SPH particles used for memoryallocation of SPH symmetric planes ghost nodes generation process (default is 100%). Recommended for large SPH particles models (value range 10~20) to control the memory allocation for SPH ghost particles with *BOUNDARY_SPH_SYMMETRY_PLANE keyword.
          * - :py:attr:`~ithk`
            - Get or set the Contact thickness option:
          * - :py:attr:`~istab`
            - Get or set the Stabilization type, only used when IFORM = 12:
          * - :py:attr:`~ql`
            - Get or set the Quasi-Linear coefficient, only used when IFORM = 12. See Remark 5.
          * - :py:attr:`~sphsort`
            - Get or set the For the implicit solver, sort and move SPH nodes from *NODE list to the end of the list.
          * - :py:attr:`~ishift`
            - Get or set the Flag for applying the shifting algorithm to SPH particles (available from R13.0). With the shifting algorithm, particles are advanced and then shifted slightly across streamlines. This process reduces particle clustering in the maximum compression and stretching directions.


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

    from control_sph import ControlSph

Property detail
---------------

.. py:property:: ncbs
   :type: int


   
   Get or set the Number of time steps between particle sorting.
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: Optional[int]


   
   Get or set the SPH approximations are computed inside a specified box(see DEFINE_BOX). When a particle has gone outside the BOX, it is deactivated. This will save computational time by eliminating particles that no longer interact with the structure.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time. Determines when the SPH calculations are stopped.
















   ..
       !! processed by numpydoc !!

.. py:property:: idim
   :type: Optional[int]


   
   Get or set the Space dimension for SPH particles:
   EQ. 3: for 3D problems
   EQ. 2: for 2D plane strain problems
   EQ.-2: for 2D axisymmetric problems
















   ..
       !! processed by numpydoc !!

.. py:property:: nmneigh
   :type: int


   
   Get or set the Defines the initial number of neighbors per particle. This variable is just for memory allocation of arrays during the initialization phase. During the calculation, some particles can request more neighbors and LS-DYNA will automatically adapt the size of that variable. Default value should apply for most applications.
















   ..
       !! processed by numpydoc !!

.. py:property:: form
   :type: int


   
   Get or set the Particle approximation theory (Remark 2):
   EQ.0:   default formulation
   EQ.1 : renormalization approximation
   EQ.2 : symmetric formulation
   EQ.3 : symmetric renormalized approximation
   EQ.4 : tensor formulation
   EQ.5 : fluid particle approximation
   EQ.6 : fluid particle with renormalization approximation
   EQ.7 : total Lagrangian formulation
   EQ.8 : total Lagrangian formulation with renormalization
   EQ.9 : adaptive SPH formulation(ASPH) with anisotropic smoothing tensor(Remark 2g)
   EQ.10 : renormalization approximation for adaptive SPH formulation(ASPH) with anisotropic smoothing tensor
   EQ.12 : moving least - squares based formulation(MPP only, see Remark 2e)
   EQ.13 : implicit incompressible formulation. (MPP only).
   EQ.15 : enhanced fluid formulation
   EQ.16 : enhanced fluid formulation with renormalization
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Start time for particle approximation. Particle approximations will be computed when time of the analysis has reached the value defined in START.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxv
   :type: float


   
   Get or set the Maximum value for velocity for the SPH particles. Particles with a velocity greater than MAXV are deactivated.
















   ..
       !! processed by numpydoc !!

.. py:property:: cont
   :type: int


   
   Get or set the Defines the computation of the particle approximation between two different SPH parts:
   EQ. 0: Particle approximation is defined (default),
   EQ. 1: Particle approximation is not computed. Two different SPH materials will not interact with each others and penetration is allowed.
















   ..
       !! processed by numpydoc !!

.. py:property:: deriv
   :type: int


   
   Get or set the Time integration type for the smoothing length. Please refer to 7.99 (CONTROL) on v970 LS-DYNA manual for EQ.0 and EQ.1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ini
   :type: int


   
   Get or set the Computation of the smoothing length during the initialization:
   EQ.0: Bucket sort based algorithm (default, very fast).
   EQ.1: Global computation on all the particles of the model.
   EQ.2: Based on the mass of the SPH particle..
















   ..
       !! processed by numpydoc !!

.. py:property:: ishow
   :type: int


   
   Get or set the Display options for SPH particles:
   EQ.0: Show all SPH particles in LS-PrePost.
   EQ.1: Exclude deactivated SPH particles in LS-PrePost
















   ..
       !! processed by numpydoc !!

.. py:property:: ierod
   :type: int


   
   Get or set the Deactivation control for SPH particles:
   EQ.0: Particles remain active.
   EQ.1: SPH particles are deactivated and stress states are set to 0 when erosion criteria are satisfied.
   EQ.2:  SPH particles are totally deactivated and stress states are set to 0 when erosion criteria are satisfied.  See Remark 3.
   EQ.3:  SPH particles are totally deactivated and stress states are set to 0 when erosion criteria are satisfied If an EOS is defined, the volumetric response is unaffected.  See Remark 3
















   ..
       !! processed by numpydoc !!

.. py:property:: icont
   :type: int


   
   Get or set the Controls contact behavior for deactivated SPH particles:
   EQ.0: Any contact defined for SPH remains active for deactivated particles.
   EQ.1: Contact is inactive for deactivated particles
















   ..
       !! processed by numpydoc !!

.. py:property:: iavis
   :type: int


   
   Get or set the Defines artificial viscosity formulation for SPH elements (Remark 3):
   EQ.0: Monaghan type artificial viscosity formulation is used.
   EQ.1: Standard type artificial viscosity formulation from solid element is used (this option is not supported in SPH 2D and 2D axisymmetric elements).
















   ..
       !! processed by numpydoc !!

.. py:property:: isymp
   :type: int


   
   Get or set the Defines the percentage of original SPH particles used for memoryallocation of SPH symmetric planes ghost nodes generation process (default is 100%). Recommended for large SPH particles models (value range 10~20) to control the memory allocation for SPH ghost particles with *BOUNDARY_SPH_SYMMETRY_PLANE keyword.
















   ..
       !! processed by numpydoc !!

.. py:property:: ithk
   :type: int


   
   Get or set the Contact thickness option:
   EQ.0:   the contact thickness is set to zero(default).
   EQ.1 : the contact thickness is automatically calculated based on the volume of each SPH particle.
   This contact thickness calculation is ignored if a non - zero contact thickness for slave surface(SST) is provided by the contact card.
















   ..
       !! processed by numpydoc !!

.. py:property:: istab
   :type: int


   
   Get or set the Stabilization type, only used when IFORM = 12:
   EQ.0:   incremental stabilization(default).Adequate for most materials.
   EQ.1 : total stabilization.Only recommended for hyperelastic materials.
















   ..
       !! processed by numpydoc !!

.. py:property:: ql
   :type: float


   
   Get or set the Quasi-Linear coefficient, only used when IFORM = 12. See Remark 5.
















   ..
       !! processed by numpydoc !!

.. py:property:: sphsort
   :type: int


   
   Get or set the For the implicit solver, sort and move SPH nodes from *NODE list to the end of the list.
   EQ.0:   no sorting(default)
   EQ.1 : perform sorting.
















   ..
       !! processed by numpydoc !!

.. py:property:: ishift
   :type: int


   
   Get or set the Flag for applying the shifting algorithm to SPH particles (available from R13.0). With the shifting algorithm, particles are advanced and then shifted slightly across streamlines. This process reduces particle clustering in the maximum compression and stretching directions.
   EQ.0:   Do not apply shifting algorithm applied(default).
   EQ.1:   Apply shifting algorithm applied.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'SPH'






