





:class:`ConstrainedSoilPileCurves`
==================================


.. py:class:: constrained_soil_pile_curves.ConstrainedSoilPileCurves(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_SOIL_PILE_CURVES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedSoilPileCurves

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pbsid`
            - Get or set the Part set ID containing beam elements for coupling (the piles).
          * - :py:attr:`~diam`
            - Get or set the Pile diameter (optional). If zero or blank, the pile diameter will be taken automatically from the section properties of the beam element.
          * - :py:attr:`~pidns`
            - Get or set the ID for automatically generated part containing visualization elements for perpendicular and axial coupling.
          * - :py:attr:`~pidnb`
            - Get or set the ID for automatically generated part containing visualization elements for base coupling.
          * - :py:attr:`~error`
            - Get or set the Action taken if any coupling point is not constrained within a soil element:
          * - :py:attr:`~nring`
            - Get or set the Number of coupling points around circumference at each pile node:
          * - :py:attr:`~nringb`
            - Get or set the Number of extra rings of coupling points on base, in addition to those around the pile circumference. By default, NRINGB is chosen automatically to distribute the base stress as uniformly as possible .
          * - :py:attr:`~damp`
            - Get or set the Optional damping coefficient for Axial coupling (stress/velocity units). An additional axial coupling shear stress equal to DAMP times the axial velocity of the pile relative to the soil will be generated.
          * - :py:attr:`~local`
            - Get or set the Flag to identify which free end of a pile is treated as the Base:
          * - :py:attr:`~pid`
            - Get or set the Part ID (depending on OPTION2) containing solid elements for coupling (the soil).
          * - :py:attr:`~zref`
            - Get or set the Reference Z-coordinate, used in calculation of “relative z-coordinate”. For example, ZREF may be located at the soil surface.  .
          * - :py:attr:`~blcz`
            - Get or set the For base coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
          * - :py:attr:`~blc`
            - Get or set the For base coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement.
          * - :py:attr:`~blcsh`
            - Get or set the For base coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate .
          * - :py:attr:`~blcsv`
            - Get or set the For base coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate .
          * - :py:attr:`~vlcz`
            - Get or set the For axial coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
          * - :py:attr:`~vlc`
            - Get or set the For axial coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement .
          * - :py:attr:`~vlcsh`
            - Get or set the For axial coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate.
          * - :py:attr:`~vlcsv`
            - Get or set the For axial coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.
          * - :py:attr:`~hlcz`
            - Get or set the For perpendicular coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
          * - :py:attr:`~hlc`
            - Get or set the For perpendicular coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement
          * - :py:attr:`~hlcsh`
            - Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate
          * - :py:attr:`~hlcsv`
            - Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.


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

    from constrained_soil_pile_curves import ConstrainedSoilPileCurves

Property detail
---------------

.. py:property:: pbsid
   :type: Optional[int]


   
   Get or set the Part set ID containing beam elements for coupling (the piles).
















   ..
       !! processed by numpydoc !!

.. py:property:: diam
   :type: Optional[float]


   
   Get or set the Pile diameter (optional). If zero or blank, the pile diameter will be taken automatically from the section properties of the beam element.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidns
   :type: Optional[int]


   
   Get or set the ID for automatically generated part containing visualization elements for perpendicular and axial coupling.
   If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidnb
   :type: Optional[int]


   
   Get or set the ID for automatically generated part containing visualization elements for base coupling.
   If not specified, LS-DYNA will assign a part ID. See Remarks 14 and 15.
















   ..
       !! processed by numpydoc !!

.. py:property:: error
   :type: int


   
   Get or set the Action taken if any coupling point is not constrained within a soil element:
   EQ.0:   Stop with an error message.
   EQ.1 : Warn and continue..
















   ..
       !! processed by numpydoc !!

.. py:property:: nring
   :type: int


   
   Get or set the Number of coupling points around circumference at each pile node:
   EQ.1:   One coupling point coincident with pile node
   GT.1 : NRING coupling points equally spaced around the circumference of the pile.
















   ..
       !! processed by numpydoc !!

.. py:property:: nringb
   :type: Optional[int]


   
   Get or set the Number of extra rings of coupling points on base, in addition to those around the pile circumference. By default, NRINGB is chosen automatically to distribute the base stress as uniformly as possible .
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: float


   
   Get or set the Optional damping coefficient for Axial coupling (stress/velocity units). An additional axial coupling shear stress equal to DAMP times the axial velocity of the pile relative to the soil will be generated.
















   ..
       !! processed by numpydoc !!

.. py:property:: local
   :type: int


   
   Get or set the Flag to identify which free end of a pile is treated as the Base:
   EQ.1:   End with the most negative global Z - coordinate
   EQ.2 : End which is Node 1 of the attached beam element topology.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID (depending on OPTION2) containing solid elements for coupling (the soil).
















   ..
       !! processed by numpydoc !!

.. py:property:: zref
   :type: Optional[float]


   
   Get or set the Reference Z-coordinate, used in calculation of “relative z-coordinate”. For example, ZREF may be located at the soil surface.  .
















   ..
       !! processed by numpydoc !!

.. py:property:: blcz
   :type: Optional[int]


   
   Get or set the For base coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
















   ..
       !! processed by numpydoc !!

.. py:property:: blc
   :type: Optional[int]


   
   Get or set the For base coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: blcsh
   :type: Optional[int]


   
   Get or set the For base coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate .
















   ..
       !! processed by numpydoc !!

.. py:property:: blcsv
   :type: Optional[int]


   
   Get or set the For base coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate .
















   ..
       !! processed by numpydoc !!

.. py:property:: vlcz
   :type: Optional[int]


   
   Get or set the For axial coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
















   ..
       !! processed by numpydoc !!

.. py:property:: vlc
   :type: Optional[int]


   
   Get or set the For axial coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement .
















   ..
       !! processed by numpydoc !!

.. py:property:: vlcsh
   :type: Optional[int]


   
   Get or set the For axial coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: vlcsv
   :type: Optional[int]


   
   Get or set the For axial coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: hlcz
   :type: Optional[int]


   
   Get or set the For perpendicular coupling, load curve ID defining ultimate strength (stress units) as a function of relative Z-coordinate (length units)
















   ..
       !! processed by numpydoc !!

.. py:property:: hlc
   :type: Optional[int]


   
   Get or set the For perpendicular coupling, load curve ID containing normalized mobilization curve: dimensionless factor on stress as a function of displacement
















   ..
       !! processed by numpydoc !!

.. py:property:: hlcsh
   :type: Optional[int]


   
   Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective horizontal stress (dimensionless) as a function of relative Z-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: hlcsv
   :type: Optional[int]


   
   Get or set the For perpendicular coupling, optional load curve ID containing coefficient for effective vertical stress (dimensionless) as a function of relative Z-coordinate.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'SOIL_PILE_CURVES'






