





:class:`ControlFormingTemplate`
===============================


.. py:class:: control_forming_template.ControlFormingTemplate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_TEMPLATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingTemplate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idtemp`
            - Get or set the Type of forming process (Detailed descriptions can be found in the Remark section)
          * - :py:attr:`~blkid`
            - Get or set the Part/Part Set  Id for the blank. If Type equals to 0, this ID is part ID, otherwise, it is part set id.
          * - :py:attr:`~dieid`
            - Get or set the Rigid Body 1 ID, See Figures 8.2a, 8.2b and 8.2c for more information
          * - :py:attr:`~pnch`
            - Get or set the Rigid Body 2 ID, See Figures 8.2a, 8.2b and 8.2c for more information
          * - :py:attr:`~bndu`
            - Get or set the Rigid Body 2 ID, See Figures 8.2a, 8.2b and 8.2c for more information
          * - :py:attr:`~bndl`
            - Get or set the Rigid Body 3 ID, See Figures 8.2a, 8.2b and 8.2c for more information
          * - :py:attr:`~type`
            - Get or set the 0:  REST1|REST4 are part IDs
          * - :py:attr:`~prebd`
            - Get or set the “Pull-over” distance, for 4 piece stretch draw only.  This is the travel distance of both upper and lower binder together after they are fully closed.  Typically, this distance is below 50 mm.  See Figure 0-3 for more information.
          * - :py:attr:`~lcss`
            - Get or set the If the material for the blank has not been defined, this curve will be used to define the stress-strain relation. Otherwise, this curve is ignored
          * - :py:attr:`~al_fe`
            - Get or set the This parameter is used to define blank Young's Modulus and density. If this parameter is defined, E and Density will be found by using the proper unit, which is specified below.
          * - :py:attr:`~r00`
            - Get or set the anisotropic parameters
          * - :py:attr:`~r45`
            - Get or set the anisotropic parameters
          * - :py:attr:`~r90`
            - Get or set the anisotropic parameters
          * - :py:attr:`~e`
            - Get or set the Material Young's Modulus. If AL/FE is defined, E is not necessary
          * - :py:attr:`~density`
            - Get or set the Blank density. If AL/FE is defined, this parameter is not necessary
          * - :py:attr:`~k`
            - Get or set the strength coefficient for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
          * - :py:attr:`~n`
            - Get or set the Exponent for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
          * - :py:attr:`~mtype`
            - Get or set the Material model (Only M37 is supported)
          * - :py:attr:`~unit`
            - Get or set the Unit adopted in this simulation. This unit is used to obtain proper punch velocity, acceleration, time step, and material properties
          * - :py:attr:`~thick`
            - Get or set the Blank thickness. If the blank thickness is already defined, this parameter is ignored
          * - :py:attr:`~gap`
            - Get or set the The home gap between rigid tools (see notes below). If *BOUNDARY_PRESCRIBED_RIGID_BODY is defined, this parameter is ignored
          * - :py:attr:`~fs`
            - Get or set the Friction coefficient. If contact is defined, this parameter is ignored
          * - :py:attr:`~patern`
            - Get or set the Velocity profile. If rigid body is already defined, this parameter is ignored.
          * - :py:attr:`~vmax`
            - Get or set the vector components of the described punch moving direction. The default direction is defined by VID
          * - :py:attr:`~vx`
            - Get or set the vector components of the described punch moving direction. The default direction is defined by VID
          * - :py:attr:`~vy`
            - Get or set the vector components of the described punch moving direction. The default direction is defined by VID
          * - :py:attr:`~vz`
            - Get or set the vector components of the described punch moving direction. The default direction is defined by VID
          * - :py:attr:`~vid`
            - Get or set the VID is the vector ID describing the punch moving direction.  The vector defined in VID overrides the vector defined in (VX,VY,VZ). If neither VID nor (VX,VY,VZ) is not defined, the punch is assumed to move in the negative z direction
          * - :py:attr:`~amax`
            - Get or set the The maximum allowable acceleration.
          * - :py:attr:`~lvlada`
            - Get or set the Maximum levels of adaptivity for the blank
          * - :py:attr:`~sizeada`
            - Get or set the Minimum element size for adaptivity
          * - :py:attr:`~timsada`
            - Get or set the Total number of adaptivities in this forming simulation
          * - :py:attr:`~d3plt`
            - Get or set the The total number of d3plot output


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

    from control_forming_template import ControlFormingTemplate

Property detail
---------------

.. py:property:: idtemp
   :type: int


   
   Get or set the Type of forming process (Detailed descriptions can be found in the Remark section)
   EQ. 1: 3-piece air-draw
   EQ. 2: 3-piece Toggle-draw
   EQ. 3: 4-piece draw
   EQ. 4: Springback
   EQ. 5: Trimming
















   ..
       !! processed by numpydoc !!

.. py:property:: blkid
   :type: int


   
   Get or set the Part/Part Set  Id for the blank. If Type equals to 0, this ID is part ID, otherwise, it is part set id.
















   ..
       !! processed by numpydoc !!

.. py:property:: dieid
   :type: Optional[int]


   
   Get or set the Rigid Body 1 ID, See Figures 8.2a, 8.2b and 8.2c for more information
















   ..
       !! processed by numpydoc !!

.. py:property:: pnch
   :type: Optional[int]


   
   Get or set the Rigid Body 2 ID, See Figures 8.2a, 8.2b and 8.2c for more information
















   ..
       !! processed by numpydoc !!

.. py:property:: bndu
   :type: Optional[int]


   
   Get or set the Rigid Body 2 ID, See Figures 8.2a, 8.2b and 8.2c for more information
















   ..
       !! processed by numpydoc !!

.. py:property:: bndl
   :type: Optional[int]


   
   Get or set the Rigid Body 3 ID, See Figures 8.2a, 8.2b and 8.2c for more information
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the 0:  REST1|REST4 are part IDs
   1:   REST1|REST4 are PART SET IDs
















   ..
       !! processed by numpydoc !!

.. py:property:: prebd
   :type: Optional[float]


   
   Get or set the “Pull-over” distance, for 4 piece stretch draw only.  This is the travel distance of both upper and lower binder together after they are fully closed.  Typically, this distance is below 50 mm.  See Figure 0-3 for more information.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the If the material for the blank has not been defined, this curve will be used to define the stress-strain relation. Otherwise, this curve is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: al_fe
   :type: str


   
   Get or set the This parameter is used to define blank Young's Modulus and density. If this parameter is defined, E and Density will be found by using the proper unit, which is specified below.
   EQ. A:  the blank is aluminum
   EQ. F:   the blank is steel (default)
















   ..
       !! processed by numpydoc !!

.. py:property:: r00
   :type: float


   
   Get or set the anisotropic parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: r45
   :type: float


   
   Get or set the anisotropic parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: r90
   :type: float


   
   Get or set the anisotropic parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Material Young's Modulus. If AL/FE is defined, E is not necessary
















   ..
       !! processed by numpydoc !!

.. py:property:: density
   :type: Optional[float]


   
   Get or set the Blank density. If AL/FE is defined, this parameter is not necessary
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the strength coefficient for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Exponent for exponential hardening. If LCSS is defined, or blank materials has already been defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the Material model (Only M37 is supported)
















   ..
       !! processed by numpydoc !!

.. py:property:: unit
   :type: int


   
   Get or set the Unit adopted in this simulation. This unit is used to obtain proper punch velocity, acceleration, time step, and material properties
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Blank thickness. If the blank thickness is already defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: float


   
   Get or set the The home gap between rigid tools (see notes below). If *BOUNDARY_PRESCRIBED_RIGID_BODY is defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Friction coefficient. If contact is defined, this parameter is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: patern
   :type: int


   
   Get or set the Velocity profile. If rigid body is already defined, this parameter is ignored.
   1.  Ramped velocity profile
   2.  Smooth velocity curve
















   ..
       !! processed by numpydoc !!

.. py:property:: vmax
   :type: float


   
   Get or set the vector components of the described punch moving direction. The default direction is defined by VID
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the vector components of the described punch moving direction. The default direction is defined by VID
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the vector components of the described punch moving direction. The default direction is defined by VID
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the vector components of the described punch moving direction. The default direction is defined by VID
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the VID is the vector ID describing the punch moving direction.  The vector defined in VID overrides the vector defined in (VX,VY,VZ). If neither VID nor (VX,VY,VZ) is not defined, the punch is assumed to move in the negative z direction
















   ..
       !! processed by numpydoc !!

.. py:property:: amax
   :type: float


   
   Get or set the The maximum allowable acceleration.
















   ..
       !! processed by numpydoc !!

.. py:property:: lvlada
   :type: Optional[int]


   
   Get or set the Maximum levels of adaptivity for the blank
















   ..
       !! processed by numpydoc !!

.. py:property:: sizeada
   :type: Optional[float]


   
   Get or set the Minimum element size for adaptivity
















   ..
       !! processed by numpydoc !!

.. py:property:: timsada
   :type: int


   
   Get or set the Total number of adaptivities in this forming simulation
















   ..
       !! processed by numpydoc !!

.. py:property:: d3plt
   :type: int


   
   Get or set the The total number of d3plot output
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_TEMPLATE'






