





:class:`EmMat002`
=================


.. py:class:: em_mat_002.EmMat002(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_MAT_002 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmMat002

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified (see *PART)
          * - :py:attr:`~mtype`
            - Get or set the Electromagnetism type of the material:
          * - :py:attr:`~sigma`
            - Get or set the Initial electrical conductivity of the material.
          * - :py:attr:`~eosid`
            - Get or set the ID of the EOS to be used for the electrical conductivity. (see *EM_EOS card).
          * - :py:attr:`~murel`
            - Get or set the Relative permeability which is the ratio of the permeability of a specific medium to the permeability of free space :ur = u/u0.
          * - :py:attr:`~eosmu`
            - Get or set the ID of the EOS to be used to define the nonlinear behavior of by an equation of state.
          * - :py:attr:`~deatht`
            - Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and will be removed from the EM solve. If a negative value is entered, a *DEFINE_FUNCTION will be expected. The following parameters are allowed: (vx, vy, vz, temp, vol, mass, Ex, Ey, Ez, Bx, By, Bz, Fx, Fy, Fz, JHrate, time). Fx, Fy, and Fz refer to the components of the Lorentz force vector. A negative value returned by the *DEFINE_FUNCTION corresponds to a 'dead' or inactive element. Once an element has been removed from the EM solve, it cannot return
          * - :py:attr:`~eosid2`
            - Get or set the Optional ID of the EOS for specifying the behavior of u by an equation of state. See *EM_EOS_TABULATED1 and *EM_EOS_TABULATED2


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

    from em_mat_002 import EmMat002

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified (see *PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Electromagnetism type of the material:
   EQ.0: Air or vacuum
   EQ.1: Insulator material: these materials have the same electromagnetism behavior asM TYPE=0
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


   
   Get or set the ID of the EOS to be used for the electrical conductivity. (see *EM_EOS card).
















   ..
       !! processed by numpydoc !!

.. py:property:: murel
   :type: Optional[float]


   
   Get or set the Relative permeability which is the ratio of the permeability of a specific medium to the permeability of free space :ur = u/u0.
















   ..
       !! processed by numpydoc !!

.. py:property:: eosmu
   :type: Optional[int]


   
   Get or set the ID of the EOS to be used to define the nonlinear behavior of by an equation of state.
















   ..
       !! processed by numpydoc !!

.. py:property:: deatht
   :type: float


   
   Get or set the Death time for the material. After DEATHT, the material will no longer be considered a conductor and will be removed from the EM solve. If a negative value is entered, a *DEFINE_FUNCTION will be expected. The following parameters are allowed: (vx, vy, vz, temp, vol, mass, Ex, Ey, Ez, Bx, By, Bz, Fx, Fy, Fz, JHrate, time). Fx, Fy, and Fz refer to the components of the Lorentz force vector. A negative value returned by the *DEFINE_FUNCTION corresponds to a 'dead' or inactive element. Once an element has been removed from the EM solve, it cannot return
















   ..
       !! processed by numpydoc !!

.. py:property:: eosid2
   :type: Optional[int]


   
   Get or set the Optional ID of the EOS for specifying the behavior of u by an equation of state. See *EM_EOS_TABULATED1 and *EM_EOS_TABULATED2
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'MAT_002'






