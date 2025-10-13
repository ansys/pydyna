





:class:`SectionFpd`
===================


.. py:class:: section_fpd.SectionFpd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_FPD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionFpd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~elform`
            - Get or set the Element formulation options.
          * - :py:attr:`~dx`
            - Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
          * - :py:attr:`~dy`
            - Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
          * - :py:attr:`~dz`
            - Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
          * - :py:attr:`~kernel`
            - Get or set the Kernel type. KERNEL=0 is for Updated Lagrangian (UL) kernel. Currently, only UL kernel is supported
          * - :py:attr:`~tstart`
            - Get or set the Starting time for the fully implicit ISPG iterations. Before TSTART, only 10 ISPG iterations are done in each structural implicit step to guarantee the fluid moves with the solid boundaries. After TSTART, the ISPG will do a full iteration in the structural implicit step. This option is very useful for cases where the structural simulation time is very long (e.g. in seconds or minutes), while the reflow process to a steady state is very short. With this option, we can let the full ISPG iteration start from TSTART and save some computational resources
          * - :py:attr:`~dt_imp`
            - Get or set the Reset the implicit structural time step size to DT_IMP after TSTART. Because the solder reflow process is very fast, a small implicit structural time step size is needed. Generally, the value of DT_IMP should be around 10~50 times of ISPG time step size to guarantee the convergence of the solution if the gravity-driven simulation is deployed.This field is optional
          * - :py:attr:`~dtscl`
            - Get or set the The time step size scaling factor for ISPG iteration. We recommend a value between 0.1~0.5. Large DTSCL may cause contact detection issues.
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

    from section_fpd import SectionFpd

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: elform
   :type: Optional[int]


   
   Get or set the Element formulation options.
   EQ.49:  Incompressible smoothed particle Galerkin formulation
















   ..
       !! processed by numpydoc !!

.. py:property:: dx
   :type: Optional[float]


   
   Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
















   ..
       !! processed by numpydoc !!

.. py:property:: dy
   :type: Optional[float]


   
   Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
















   ..
       !! processed by numpydoc !!

.. py:property:: dz
   :type: Optional[float]


   
   Get or set the Normalized dilation parameters of the kernel function in x, y and z directions.  The normalized dilation parameters of the kernel function are introduced to provide the smoothness and compact support properties on the construction of the mesh-free shape functions.  Values between 1.5 and 1.8 are recommended. The nodal support size of particles will be automatically adjusted with the material’s deformation, but it is not allowed to be decreased.
















   ..
       !! processed by numpydoc !!

.. py:property:: kernel
   :type: Optional[int]


   
   Get or set the Kernel type. KERNEL=0 is for Updated Lagrangian (UL) kernel. Currently, only UL kernel is supported
















   ..
       !! processed by numpydoc !!

.. py:property:: tstart
   :type: float


   
   Get or set the Starting time for the fully implicit ISPG iterations. Before TSTART, only 10 ISPG iterations are done in each structural implicit step to guarantee the fluid moves with the solid boundaries. After TSTART, the ISPG will do a full iteration in the structural implicit step. This option is very useful for cases where the structural simulation time is very long (e.g. in seconds or minutes), while the reflow process to a steady state is very short. With this option, we can let the full ISPG iteration start from TSTART and save some computational resources
















   ..
       !! processed by numpydoc !!

.. py:property:: dt_imp
   :type: Optional[float]


   
   Get or set the Reset the implicit structural time step size to DT_IMP after TSTART. Because the solder reflow process is very fast, a small implicit structural time step size is needed. Generally, the value of DT_IMP should be around 10~50 times of ISPG time step size to guarantee the convergence of the solution if the gravity-driven simulation is deployed.This field is optional
















   ..
       !! processed by numpydoc !!

.. py:property:: dtscl
   :type: float


   
   Get or set the The time step size scaling factor for ISPG iteration. We recommend a value between 0.1~0.5. Large DTSCL may cause contact detection issues.
















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
   :value: 'SECTION'


.. py:attribute:: subkeyword
   :value: 'FPD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





