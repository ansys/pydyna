





:class:`ControlRemeshingEfg`
============================


.. py:class:: control_remeshing_efg.ControlRemeshingEfg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_REMESHING_EFG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlRemeshingEfg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rmin`
            - Get or set the Minimum edge length for the surface mesh surrounding the parts which should be remeshed.
          * - :py:attr:`~rmax`
            - Get or set the Maximum edge length for the surface mesh surrounding the parts which should be remeshed.
          * - :py:attr:`~vf_loss`
            - Get or set the Volume fraction loss required in a type 10/13 tetrahedral elements to trigger a remesh.  For the type 13 solid elements, the pressures are computed at the nodal points; therefore, it is possible for overall volume to be conserved but for individual tetrahedrons to experience a significant volume loss or gain.  The volume loss can lead to numerical problems.  Recommended values for VF_‌LOSS in the range of 0.10 to 0.30 may be reasonable.
          * - :py:attr:`~mfrac`
            - Get or set the Mass ratio gain during mass scaling required for triggering a remesh. For a one percent increase in mass, set MFRAC=0.01.
          * - :py:attr:`~dt_min`
            - Get or set the Time-step size required for triggering a remesh. This option is checked before mass scaling is applied and the time step size reset.
          * - :py:attr:`~icurv`
            - Get or set the Define number of element along the radius in the adaptivity.
          * - :py:attr:`~iadp10`
            - Get or set the Not used.
          * - :py:attr:`~sefang`
            - Get or set the Define angular mesh size in 3-D axisymmetric remeshing.
          * - :py:attr:`~ivt`
            - Get or set the Internal variable transfer in adaptive EFG
          * - :py:attr:`~iat`
            - Get or set the Interactive adaptivity
          * - :py:attr:`~iaat`
            - Get or set the Interactive adaptivity adjustable tolerance
          * - :py:attr:`~mm`
            - Get or set the Interactive adaptive remeshing with monotonic resizing:
          * - :py:attr:`~iat1`
            - Get or set the Define the tolerance of shear distortion indicator for interactive adaptivity. (0.1~0.5 is recommended)
          * - :py:attr:`~iat2`
            - Get or set the Define the tolerance of unbalanced nodal distribution indicator for interactive adaptivity. (RMAX/RMIN is recommended)
          * - :py:attr:`~iat3`
            - Get or set the Define the tolerance of volumetric change indicator for interactive adaptivity.


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

    from control_remeshing_efg import ControlRemeshingEfg

Property detail
---------------

.. py:property:: rmin
   :type: Optional[float]


   
   Get or set the Minimum edge length for the surface mesh surrounding the parts which should be remeshed.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmax
   :type: Optional[float]


   
   Get or set the Maximum edge length for the surface mesh surrounding the parts which should be remeshed.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf_loss
   :type: float


   
   Get or set the Volume fraction loss required in a type 10/13 tetrahedral elements to trigger a remesh.  For the type 13 solid elements, the pressures are computed at the nodal points; therefore, it is possible for overall volume to be conserved but for individual tetrahedrons to experience a significant volume loss or gain.  The volume loss can lead to numerical problems.  Recommended values for VF_‌LOSS in the range of 0.10 to 0.30 may be reasonable.
















   ..
       !! processed by numpydoc !!

.. py:property:: mfrac
   :type: float


   
   Get or set the Mass ratio gain during mass scaling required for triggering a remesh. For a one percent increase in mass, set MFRAC=0.01.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt_min
   :type: float


   
   Get or set the Time-step size required for triggering a remesh. This option is checked before mass scaling is applied and the time step size reset.
















   ..
       !! processed by numpydoc !!

.. py:property:: icurv
   :type: int


   
   Get or set the Define number of element along the radius in the adaptivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: iadp10
   :type: int


   
   Get or set the Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sefang
   :type: float


   
   Get or set the Define angular mesh size in 3-D axisymmetric remeshing.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivt
   :type: int


   
   Get or set the Internal variable transfer in adaptive EFG
   EQ.1: Moving Least square approximation with Kronecker-delta property
   EQ.-1: Moving Least square approximation without Kronecker-delta property.
   EQ.2: Partition of unity approximation with Kronecker-delta property.
   EQ.-2: Partition of unity approximation without Kronecker-delta property.
















   ..
       !! processed by numpydoc !!

.. py:property:: iat
   :type: int


   
   Get or set the Interactive adaptivity
   EQ. 0: No interactive adaptivity.
   EQ. 1: Interactive adaptivity combined with predefined adaptivity.
   EQ. 2: Purely interactive adaptivity. The time interval between two successive adaptive steps is bounded by ADPFREQ.
   EQ. 3: Purely interactive adaptivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: iaat
   :type: int


   
   Get or set the Interactive adaptivity adjustable tolerance
   EQ. 0: The tolerance to trigger interactive adaptivity is not adjusted.
   EQ. 1: The tolerance is adjusted in run-time to avoid over-activation.
















   ..
       !! processed by numpydoc !!

.. py:property:: mm
   :type: int


   
   Get or set the Interactive adaptive remeshing with monotonic resizing:
   EQ.1:   the adaptive remeshing cannot coarsen a mesh.The current implementation only supports IAT = 1, 2, 3 and IER = 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: iat1
   :type: float


   
   Get or set the Define the tolerance of shear distortion indicator for interactive adaptivity. (0.1~0.5 is recommended)
















   ..
       !! processed by numpydoc !!

.. py:property:: iat2
   :type: float


   
   Get or set the Define the tolerance of unbalanced nodal distribution indicator for interactive adaptivity. (RMAX/RMIN is recommended)
















   ..
       !! processed by numpydoc !!

.. py:property:: iat3
   :type: float


   
   Get or set the Define the tolerance of volumetric change indicator for interactive adaptivity.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'REMESHING_EFG'






