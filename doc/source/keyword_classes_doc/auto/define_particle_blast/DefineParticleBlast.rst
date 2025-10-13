





:class:`DefineParticleBlast`
============================


.. py:class:: define_particle_blast.DefineParticleBlast(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_PARTICLE_BLAST keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineParticleBlast

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lagsid`
            - Get or set the Structure ID for particle structure interaction
          * - :py:attr:`~lagstype`
            - Get or set the Structure type:
          * - :py:attr:`~nodid`
            - Get or set the Discrete element sphere (DES) or Smooth particle hydrodynamics (SPH) ID for the interaction between particles and nodes
          * - :py:attr:`~nodtype`
            - Get or set the Nodal type:
          * - :py:attr:`~hecid`
            - Get or set the Initial container for high explosive particle
          * - :py:attr:`~hectype`
            - Get or set the Structure type:
          * - :py:attr:`~aircid`
            - Get or set the Initial geometry for air particles:
          * - :py:attr:`~nphe`
            - Get or set the Number of high explosive particles
          * - :py:attr:`~npair`
            - Get or set the Number of air particles
          * - :py:attr:`~iunit`
            - Get or set the Unit System
          * - :py:attr:`~ihetype`
            - Get or set the High Explosive type (see Remark 1):
          * - :py:attr:`~density`
            - Get or set the High Explosive density for user defined explosive
          * - :py:attr:`~energy`
            - Get or set the High Explosive energy per unit volume for user defined explosive
          * - :py:attr:`~gamma`
            - Get or set the High Explosive fraction between C_p and C_v for user defined explosive
          * - :py:attr:`~covol`
            - Get or set the High Explosive co-volume for user defined explosive
          * - :py:attr:`~deto_v`
            - Get or set the High Explosive detonation velocity for user define explosive
          * - :py:attr:`~detx`
            - Get or set the Detonation point x
          * - :py:attr:`~dety`
            - Get or set the Detonation point y
          * - :py:attr:`~detz`
            - Get or set the Detonation point z
          * - :py:attr:`~tdet`
            - Get or set the Detonation time
          * - :py:attr:`~btend`
            - Get or set the Blast end time
          * - :py:attr:`~nid`
            - Get or set the An optional node ID defining the position of the detonation point. If defined, its coordinates will overwrite the DETX, DETY, and DETZ defined above.
          * - :py:attr:`~bcx0`
            - Get or set the Global domain x-min
          * - :py:attr:`~bcx1`
            - Get or set the Global domain x-max
          * - :py:attr:`~bcy0`
            - Get or set the Global domain y-min
          * - :py:attr:`~bcy1`
            - Get or set the Global domain y-max
          * - :py:attr:`~bcz0`
            - Get or set the Global domain z-min
          * - :py:attr:`~bcz1`
            - Get or set the Global domain y-max
          * - :py:attr:`~ibcx0`
            - Get or set the Boundary conditions for global domain x-min:
          * - :py:attr:`~ibcx1`
            - Get or set the Boundary conditions for global domain x-max:
          * - :py:attr:`~ibcy0`
            - Get or set the Boundary conditions for global domain y-min:
          * - :py:attr:`~ibcy1`
            - Get or set the Boundary conditions for global domain y-max:
          * - :py:attr:`~ibcz0`
            - Get or set the Boundary conditions for global domain z-min:
          * - :py:attr:`~ibcz1`
            - Get or set the Boundary conditions for global domain z-max:
          * - :py:attr:`~bc_p`
            - Get or set the Pressure ambient boundary condition for global domain:
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

    from define_particle_blast import DefineParticleBlast

Property detail
---------------

.. py:property:: lagsid
   :type: Optional[int]


   
   Get or set the Structure ID for particle structure interaction
















   ..
       !! processed by numpydoc !!

.. py:property:: lagstype
   :type: int


   
   Get or set the Structure type:
   EQ.0:   Part Set
   EQ.1 : Part
















   ..
       !! processed by numpydoc !!

.. py:property:: nodid
   :type: Optional[int]


   
   Get or set the Discrete element sphere (DES) or Smooth particle hydrodynamics (SPH) ID for the interaction between particles and nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: nodtype
   :type: int


   
   Get or set the Nodal type:
   EQ.0:   Node Set
   EQ.1 : Node
   EQ.2 : Part Set
   EQ.3 : Part
















   ..
       !! processed by numpydoc !!

.. py:property:: hecid
   :type: Optional[int]


   
   Get or set the Initial container for high explosive particle
















   ..
       !! processed by numpydoc !!

.. py:property:: hectype
   :type: int


   
   Get or set the Structure type:
   EQ.0:   Part Set
   EQ.1 : Part
   EQ.2 : Geometry, see* DEFINE_‌PBLAST_‌GEOMETRY
















   ..
       !! processed by numpydoc !!

.. py:property:: aircid
   :type: Optional[int]


   
   Get or set the Initial geometry for air particles:
   EQ.0:   Filled air particles to entire domain defined by Card 5
   GT.0 : Reference to * DEFINE_‌PBLAST_‌AIRGEO ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nphe
   :type: Optional[int]


   
   Get or set the Number of high explosive particles
















   ..
       !! processed by numpydoc !!

.. py:property:: npair
   :type: Optional[int]


   
   Get or set the Number of air particles
















   ..
       !! processed by numpydoc !!

.. py:property:: iunit
   :type: int


   
   Get or set the Unit System
   EQ.0:   Kg - mm - ms - K
   EQ.1 : SI Units
   EQ.2 : Ton - mm - s - K
   EQ.3 : g - cm - us - K
   EQ.4 : blob - in - s - K
















   ..
       !! processed by numpydoc !!

.. py:property:: ihetype
   :type: int


   
   Get or set the High Explosive type (see Remark 1):
   EQ.0:   User defined
   EQ.1 : TNT
   EQ.2 : C4
















   ..
       !! processed by numpydoc !!

.. py:property:: density
   :type: Optional[float]


   
   Get or set the High Explosive density for user defined explosive
















   ..
       !! processed by numpydoc !!

.. py:property:: energy
   :type: Optional[float]


   
   Get or set the High Explosive energy per unit volume for user defined explosive
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the High Explosive fraction between C_p and C_v for user defined explosive
















   ..
       !! processed by numpydoc !!

.. py:property:: covol
   :type: Optional[float]


   
   Get or set the High Explosive co-volume for user defined explosive
















   ..
       !! processed by numpydoc !!

.. py:property:: deto_v
   :type: Optional[float]


   
   Get or set the High Explosive detonation velocity for user define explosive
















   ..
       !! processed by numpydoc !!

.. py:property:: detx
   :type: Optional[float]


   
   Get or set the Detonation point x
















   ..
       !! processed by numpydoc !!

.. py:property:: dety
   :type: Optional[float]


   
   Get or set the Detonation point y
















   ..
       !! processed by numpydoc !!

.. py:property:: detz
   :type: Optional[float]


   
   Get or set the Detonation point z
















   ..
       !! processed by numpydoc !!

.. py:property:: tdet
   :type: Optional[float]


   
   Get or set the Detonation time
















   ..
       !! processed by numpydoc !!

.. py:property:: btend
   :type: Optional[float]


   
   Get or set the Blast end time
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the An optional node ID defining the position of the detonation point. If defined, its coordinates will overwrite the DETX, DETY, and DETZ defined above.
















   ..
       !! processed by numpydoc !!

.. py:property:: bcx0
   :type: Optional[float]


   
   Get or set the Global domain x-min
















   ..
       !! processed by numpydoc !!

.. py:property:: bcx1
   :type: Optional[float]


   
   Get or set the Global domain x-max
















   ..
       !! processed by numpydoc !!

.. py:property:: bcy0
   :type: Optional[float]


   
   Get or set the Global domain y-min
















   ..
       !! processed by numpydoc !!

.. py:property:: bcy1
   :type: Optional[float]


   
   Get or set the Global domain y-max
















   ..
       !! processed by numpydoc !!

.. py:property:: bcz0
   :type: Optional[float]


   
   Get or set the Global domain z-min
















   ..
       !! processed by numpydoc !!

.. py:property:: bcz1
   :type: Optional[float]


   
   Get or set the Global domain y-max
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcx0
   :type: Optional[int]


   
   Get or set the Boundary conditions for global domain x-min:
   EQ.0:   Free
   EQ.1 : Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcx1
   :type: Optional[int]


   
   Get or set the Boundary conditions for global domain x-max:
   EQ.0:   Free
   EQ.1 : Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcy0
   :type: Optional[int]


   
   Get or set the Boundary conditions for global domain y-min:
   EQ.0:   Free
   EQ.1 : Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcy1
   :type: Optional[int]


   
   Get or set the Boundary conditions for global domain y-max:
   EQ.0:   Free
   EQ.1 : Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcz0
   :type: Optional[int]


   
   Get or set the Boundary conditions for global domain z-min:
   EQ.0:   Free
   EQ.1 : Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcz1
   :type: Optional[int]


   
   Get or set the Boundary conditions for global domain z-max:
   EQ.0:   Free
   EQ.1 : Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: bc_p
   :type: int


   
   Get or set the Pressure ambient boundary condition for global domain:
   EQ.0:   Off(Default)
   EQ.1 : On
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'PARTICLE_BLAST'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





