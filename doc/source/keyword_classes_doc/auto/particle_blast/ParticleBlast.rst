





:class:`ParticleBlast`
======================


.. py:class:: particle_blast.ParticleBlast(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PARTICLE_BLAST keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ParticleBlast

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lagsid`
            - Get or set the Structure id for particle structure interaction
          * - :py:attr:`~lagstype`
            - Get or set the Structure type
          * - :py:attr:`~dodid`
            - Get or set the Discrete element sphere (DES) or Smooth particle hydrodynamics (SPH) id for the interaction between particles and nodes.
          * - :py:attr:`~dodtype`
            - Get or set the Nodal type
          * - :py:attr:`~hecid`
            - Get or set the Initial container for high explosive particle
          * - :py:attr:`~hectype`
            - Get or set the Structure type
          * - :py:attr:`~aircid`
            - Get or set the Initial geometry for air particles
          * - :py:attr:`~nphe`
            - Get or set the Number of high explosive particles
          * - :py:attr:`~npair`
            - Get or set the Number of air particles
          * - :py:attr:`~iunit`
            - Get or set the Unit System
          * - :py:attr:`~ihetype`
            - Get or set the High Explosive type
          * - :py:attr:`~densit`
            - Get or set the High Explosive density
          * - :py:attr:`~energy`
            - Get or set the High Explosive energy per unit volume
          * - :py:attr:`~gamma`
            - Get or set the High Explosive fraction between Cp and Cv
          * - :py:attr:`~covol`
            - Get or set the High Explosive co-volume
          * - :py:attr:`~deto_v`
            - Get or set the High Explosive detonation velocity
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
          * - :py:attr:`~bcxo`
            - Get or set the Global domain x-min
          * - :py:attr:`~bcx1`
            - Get or set the Global domain x-max
          * - :py:attr:`~bcy0`
            - Get or set the Global domain y-min
          * - :py:attr:`~bcy1`
            - Get or set the Global domain x-max
          * - :py:attr:`~bcz0`
            - Get or set the Global domain z-min
          * - :py:attr:`~bcz1`
            - Get or set the Global domain x-max
          * - :py:attr:`~ibcx0`
            - Get or set the Boundary conditions for global domain x-min
          * - :py:attr:`~ibcx1`
            - Get or set the Boundary conditions for global domain x-max
          * - :py:attr:`~ibcy0`
            - Get or set the Boundary conditions for global domain y-min
          * - :py:attr:`~ibcy1`
            - Get or set the Boundary conditions for global domain y-max
          * - :py:attr:`~ibcz0`
            - Get or set the Boundary conditions for global domain z-min
          * - :py:attr:`~ibcz1`
            - Get or set the Boundary conditions for global domain z-max
          * - :py:attr:`~bc_p`
            - Get or set the Pressure ambient boundary condition for global domain


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

    from particle_blast import ParticleBlast

Property detail
---------------

.. py:property:: lagsid
   :type: int


   
   Get or set the Structure id for particle structure interaction
















   ..
       !! processed by numpydoc !!

.. py:property:: lagstype
   :type: int


   
   Get or set the Structure type
   EQ.0: Part Set
   EQ.1: Part
















   ..
       !! processed by numpydoc !!

.. py:property:: dodid
   :type: int


   
   Get or set the Discrete element sphere (DES) or Smooth particle hydrodynamics (SPH) id for the interaction between particles and nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: dodtype
   :type: int


   
   Get or set the Nodal type
   EQ.0: Node Set
   EQ.1: Node
















   ..
       !! processed by numpydoc !!

.. py:property:: hecid
   :type: int


   
   Get or set the Initial container for high explosive particle
















   ..
       !! processed by numpydoc !!

.. py:property:: hectype
   :type: int


   
   Get or set the Structure type
   EQ.0: Part Set
   EQ.1: Part
   EQ.2: Geometry, see *DEFINE_PBLAST_GEOMETRY
















   ..
       !! processed by numpydoc !!

.. py:property:: aircid
   :type: int


   
   Get or set the Initial geometry for air particles
   EQ.0: filled air particles to entire domain defined by Card 5
   GT.0: Reference to *DEFINE_PBLAST_AIRGEO ID
















   ..
       !! processed by numpydoc !!

.. py:property:: nphe
   :type: int


   
   Get or set the Number of high explosive particles
















   ..
       !! processed by numpydoc !!

.. py:property:: npair
   :type: int


   
   Get or set the Number of air particles
















   ..
       !! processed by numpydoc !!

.. py:property:: iunit
   :type: int


   
   Get or set the Unit System
   EQ.0: Kg-mm-ms-K
   EQ.1: SI Units
   EQ.2: Ton-mm-s-K
   EQ.3: g-cm-us-K
















   ..
       !! processed by numpydoc !!

.. py:property:: ihetype
   :type: int


   
   Get or set the High Explosive type
   EQ.1: TNT
   EQ.2: C4
   Others: Self Define
















   ..
       !! processed by numpydoc !!

.. py:property:: densit
   :type: float


   
   Get or set the High Explosive density
















   ..
       !! processed by numpydoc !!

.. py:property:: energy
   :type: float


   
   Get or set the High Explosive energy per unit volume
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: float


   
   Get or set the High Explosive fraction between Cp and Cv
















   ..
       !! processed by numpydoc !!

.. py:property:: covol
   :type: float


   
   Get or set the High Explosive co-volume
















   ..
       !! processed by numpydoc !!

.. py:property:: deto_v
   :type: float


   
   Get or set the High Explosive detonation velocity
















   ..
       !! processed by numpydoc !!

.. py:property:: detx
   :type: float


   
   Get or set the Detonation point x
















   ..
       !! processed by numpydoc !!

.. py:property:: dety
   :type: float


   
   Get or set the Detonation point y
















   ..
       !! processed by numpydoc !!

.. py:property:: detz
   :type: float


   
   Get or set the Detonation point z
















   ..
       !! processed by numpydoc !!

.. py:property:: tdet
   :type: float


   
   Get or set the Detonation time
















   ..
       !! processed by numpydoc !!

.. py:property:: btend
   :type: float


   
   Get or set the Blast end time
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: int


   
   Get or set the An optional node ID defining the position of the detonation point. If defined, its coordinates will overwrite the DETX, DETY, and DETZ defined above.
















   ..
       !! processed by numpydoc !!

.. py:property:: bcxo
   :type: float


   
   Get or set the Global domain x-min
















   ..
       !! processed by numpydoc !!

.. py:property:: bcx1
   :type: float


   
   Get or set the Global domain x-max
















   ..
       !! processed by numpydoc !!

.. py:property:: bcy0
   :type: float


   
   Get or set the Global domain y-min
















   ..
       !! processed by numpydoc !!

.. py:property:: bcy1
   :type: float


   
   Get or set the Global domain x-max
















   ..
       !! processed by numpydoc !!

.. py:property:: bcz0
   :type: float


   
   Get or set the Global domain z-min
















   ..
       !! processed by numpydoc !!

.. py:property:: bcz1
   :type: float


   
   Get or set the Global domain x-max
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcx0
   :type: int


   
   Get or set the Boundary conditions for global domain x-min
   EQ.0: Free
   EQ.1: Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcx1
   :type: int


   
   Get or set the Boundary conditions for global domain x-max
   EQ.0: Free
   EQ.1: Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcy0
   :type: int


   
   Get or set the Boundary conditions for global domain y-min
   EQ.0: Free
   EQ.1: Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcy1
   :type: int


   
   Get or set the Boundary conditions for global domain y-max
   EQ.0: Free
   EQ.1: Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcz0
   :type: int


   
   Get or set the Boundary conditions for global domain z-min
   EQ.0: Free
   EQ.1: Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: ibcz1
   :type: int


   
   Get or set the Boundary conditions for global domain z-max
   EQ.0: Free
   EQ.1: Rigid reflecting boundary
















   ..
       !! processed by numpydoc !!

.. py:property:: bc_p
   :type: int


   
   Get or set the Pressure ambient boundary condition for global domain
   EQ.0: Off (Default)
   EQ.1: On
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PARTICLE'


.. py:attribute:: subkeyword
   :value: 'BLAST'






