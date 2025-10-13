





:class:`EmMat004`
=================


.. py:class:: em_mat_004.EmMat004(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_MAT_004 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmMat004

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID: refers to MID in the *PART card.
          * - :py:attr:`~mtype`
            - Get or set the Defines the electromagnetism type of the material:
          * - :py:attr:`~sigma`
            - Get or set the Initial electrical conductivity of the material.
          * - :py:attr:`~eosid`
            - Get or set the ID of the EOS to be used for the electrical conductivity (see *EM_EOS cards).
          * - :py:attr:`~nele`
            - Get or set the Number of elements in the thickness of the shell. It is up to the user to make sure his mesh is fine enough to correctly capture the inductive-diffusive effects (see skin depth definition).
          * - :py:attr:`~murel`
            - Get or set the Relative permeability which is the ratio of the permeability of a specific medium to the permeability of free space.
          * - :py:attr:`~eosmu`
            - Get or set the ID of the EOS to be used to define the nonlinear behavior of μ. Note: if EOSMU is defined, MUREL will be used for the initial value only. See EM_EOS_PERMEABILITY.
          * - :py:attr:`~deatht`
            - Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and will be removed from the EM solve.


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

    from em_mat_004 import EmMat004

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID: refers to MID in the *PART card.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Defines the electromagnetism type of the material:
   EQ.0: Air or vacuum
   EQ.1: Insulator material: these materials have the same electromagnetism behavior as EQ.0
   EQ.2: Conductor carrying a source. In these conductors, the eddy current problem is solved, which gives the actual current density. Typically, this would correspond to the coil.
   EQ.4: Conductor not connected to any current or voltage source, where the Eddy current problem is solved. Typically, this would correspond to the workpiece
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: sigma
   :type: Optional[float]


   
   Get or set the Initial electrical conductivity of the material.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the ID of the EOS to be used for the electrical conductivity (see *EM_EOS cards).
















   ..
       !! processed by numpydoc !!

.. py:property:: nele
   :type: int


   
   Get or set the Number of elements in the thickness of the shell. It is up to the user to make sure his mesh is fine enough to correctly capture the inductive-diffusive effects (see skin depth definition).
















   ..
       !! processed by numpydoc !!

.. py:property:: murel
   :type: float


   
   Get or set the Relative permeability which is the ratio of the permeability of a specific medium to the permeability of free space.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosmu
   :type: Optional[int]


   
   Get or set the ID of the EOS to be used to define the nonlinear behavior of μ. Note: if EOSMU is defined, MUREL will be used for the initial value only. See EM_EOS_PERMEABILITY.
















   ..
       !! processed by numpydoc !!

.. py:property:: deatht
   :type: float


   
   Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and will be removed from the EM solve.
   If a negative value is entered, a *DEFINE_FUNCTION will be expected. The following parameters are allowed:
   (vx, vy, vz, temp, vol, mass, Ex, Ey, Ez, Bx, By, Bz, Fx, Fy, Fz, JHrate, time). Fx, Fy, and Fz refer to the components of the Lorentz force vector.
   A negative value returned by the *DEFINE_‌FUNCTION corresponds to a ‘dead’ or inactive element. Once an element has been removed from the EM solve, it cannot return.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'MAT_004'






