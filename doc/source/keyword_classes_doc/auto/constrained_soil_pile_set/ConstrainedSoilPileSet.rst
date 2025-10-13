





:class:`ConstrainedSoilPileSet`
===============================


.. py:class:: constrained_soil_pile_set.ConstrainedSoilPileSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_SOIL_PILE_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedSoilPileSet

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
          * - :py:attr:`~pid`
            - Get or set the Part SET ID (depending on OPTION2) containing solid elements for coupling (the soil).
          * - :py:attr:`~acu`
            - Get or set the Constant term in depth-dependence formula. Units of stress..
          * - :py:attr:`~bcu`
            - Get or set the Coefficient on relative Z-coordinate in depth-dependence formula. Units of stress/length. Note that soil strengths (and therefore coupling properties) generally increase with depth, meaning they increase with an increasingly negative Z-coordinate. Therefore, this term is usually negative..
          * - :py:attr:`~lccu`
            - Get or set the Optional load curve ID giving stress (stress units) as a function of relative Z-coordinate (length units). If defined, LCCU overrides ACU and BCU. Note that “increasing depth” corresponds to “increasingly negative relative Z-coordinate”..
          * - :py:attr:`~astiffs`
            - Get or set the Generic stiffness term. Units of stress / length.
          * - :py:attr:`~bstiffs`
            - Get or set the Generic Z-coordinate-dependent stiffness term. Units of stress / length2.
          * - :py:attr:`~astiffb`
            - Get or set the Base stiffness. Units of stress / length.
          * - :py:attr:`~zref`
            - Get or set the Reference Z-coordinate to calculate “relative Z-coordinate”.
          * - :py:attr:`~kbcon`
            - Get or set the Base coupling, constant term (stress units)
          * - :py:attr:`~kbcu`
            - Get or set the Base coupling, coefficient for Cu  (dimensionless).
          * - :py:attr:`~kbsx`
            - Get or set the Base coupling, coefficient for effective global X-stress (dimensionless).
          * - :py:attr:`~kbsy`
            - Get or set the Base coupling, coefficient for effective global Y-stress (dimensionless).
          * - :py:attr:`~kbsz`
            - Get or set the Base coupling, coefficient for effective global Z-stress (dimensionless).
          * - :py:attr:`~bstfac`
            - Get or set the Base coupling, factor on elastic stiffness (dimensionless).
          * - :py:attr:`~bhyper`
            - Get or set the Base coupling, hyperbolic curve limit (dimensionless).
          * - :py:attr:`~blc`
            - Get or set the Base coupling, load curve ID for dimensionless factor on stress as a function of displacement .
          * - :py:attr:`~kvcon`
            - Get or set the Axial coupling, constant term (stress units)
          * - :py:attr:`~kvcu`
            - Get or set the Axial coupling, coefficient for Cu  (dimensionless).
          * - :py:attr:`~kvsx`
            - Get or set the Axial coupling, coefficient for effective global X-stress (dimensionless).
          * - :py:attr:`~kvsy`
            - Get or set the Axial coupling, coefficient for effective global Y-stress (dimensionless).
          * - :py:attr:`~kvsz`
            - Get or set the Axial coupling, coefficient for effective global Z-stress (dimensionless).
          * - :py:attr:`~vstfac`
            - Get or set the Axial coupling, factor on elastic stiffness (dimensionless).
          * - :py:attr:`~vhyper`
            - Get or set the Axial coupling, hyperbolic curve limit (dimensionless).
          * - :py:attr:`~vlc`
            - Get or set the Axial coupling, load curve ID for dimensionless factor on stress as a function of displacement .
          * - :py:attr:`~khcon`
            - Get or set the Perpendicular coupling, constant term (stress units)
          * - :py:attr:`~khcu`
            - Get or set the Perpendicular coupling, coefficient for Cu  (dimensionless).
          * - :py:attr:`~khsx`
            - Get or set the Perpendicular coupling, coefficient for effective global X-stress (dimensionless).
          * - :py:attr:`~khsy`
            - Get or set the Perpendicular coupling, coefficient for effective global Y-stress (dimensionless).
          * - :py:attr:`~khsz`
            - Get or set the Perpendicular coupling, coefficient for effective global Z-stress (dimensionless).
          * - :py:attr:`~hstfac`
            - Get or set the Perpendicular coupling, factor on elastic stiffness (dimensionless).
          * - :py:attr:`~hhyper`
            - Get or set the Perpendicular coupling, hyperbolic curve limit (dimensionless).
          * - :py:attr:`~hlc`
            - Get or set the Perpendicular coupling, load curve ID for dimensionless factor on stress as a function of displacement .


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

    from constrained_soil_pile_set import ConstrainedSoilPileSet

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

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part SET ID (depending on OPTION2) containing solid elements for coupling (the soil).
















   ..
       !! processed by numpydoc !!

.. py:property:: acu
   :type: Optional[float]


   
   Get or set the Constant term in depth-dependence formula. Units of stress..
















   ..
       !! processed by numpydoc !!

.. py:property:: bcu
   :type: Optional[float]


   
   Get or set the Coefficient on relative Z-coordinate in depth-dependence formula. Units of stress/length. Note that soil strengths (and therefore coupling properties) generally increase with depth, meaning they increase with an increasingly negative Z-coordinate. Therefore, this term is usually negative..
















   ..
       !! processed by numpydoc !!

.. py:property:: lccu
   :type: Optional[int]


   
   Get or set the Optional load curve ID giving stress (stress units) as a function of relative Z-coordinate (length units). If defined, LCCU overrides ACU and BCU. Note that “increasing depth” corresponds to “increasingly negative relative Z-coordinate”..
















   ..
       !! processed by numpydoc !!

.. py:property:: astiffs
   :type: Optional[float]


   
   Get or set the Generic stiffness term. Units of stress / length.
















   ..
       !! processed by numpydoc !!

.. py:property:: bstiffs
   :type: Optional[float]


   
   Get or set the Generic Z-coordinate-dependent stiffness term. Units of stress / length2.
















   ..
       !! processed by numpydoc !!

.. py:property:: astiffb
   :type: Optional[float]


   
   Get or set the Base stiffness. Units of stress / length.
















   ..
       !! processed by numpydoc !!

.. py:property:: zref
   :type: Optional[float]


   
   Get or set the Reference Z-coordinate to calculate “relative Z-coordinate”.
















   ..
       !! processed by numpydoc !!

.. py:property:: kbcon
   :type: Optional[float]


   
   Get or set the Base coupling, constant term (stress units)
















   ..
       !! processed by numpydoc !!

.. py:property:: kbcu
   :type: Optional[float]


   
   Get or set the Base coupling, coefficient for Cu  (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: kbsx
   :type: Optional[float]


   
   Get or set the Base coupling, coefficient for effective global X-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: kbsy
   :type: Optional[float]


   
   Get or set the Base coupling, coefficient for effective global Y-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: kbsz
   :type: Optional[float]


   
   Get or set the Base coupling, coefficient for effective global Z-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: bstfac
   :type: float


   
   Get or set the Base coupling, factor on elastic stiffness (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: bhyper
   :type: Optional[float]


   
   Get or set the Base coupling, hyperbolic curve limit (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: blc
   :type: Optional[int]


   
   Get or set the Base coupling, load curve ID for dimensionless factor on stress as a function of displacement .
















   ..
       !! processed by numpydoc !!

.. py:property:: kvcon
   :type: Optional[float]


   
   Get or set the Axial coupling, constant term (stress units)
















   ..
       !! processed by numpydoc !!

.. py:property:: kvcu
   :type: Optional[float]


   
   Get or set the Axial coupling, coefficient for Cu  (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: kvsx
   :type: Optional[float]


   
   Get or set the Axial coupling, coefficient for effective global X-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: kvsy
   :type: Optional[float]


   
   Get or set the Axial coupling, coefficient for effective global Y-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: kvsz
   :type: Optional[float]


   
   Get or set the Axial coupling, coefficient for effective global Z-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: vstfac
   :type: float


   
   Get or set the Axial coupling, factor on elastic stiffness (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: vhyper
   :type: Optional[float]


   
   Get or set the Axial coupling, hyperbolic curve limit (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: vlc
   :type: Optional[int]


   
   Get or set the Axial coupling, load curve ID for dimensionless factor on stress as a function of displacement .
















   ..
       !! processed by numpydoc !!

.. py:property:: khcon
   :type: Optional[float]


   
   Get or set the Perpendicular coupling, constant term (stress units)
















   ..
       !! processed by numpydoc !!

.. py:property:: khcu
   :type: Optional[float]


   
   Get or set the Perpendicular coupling, coefficient for Cu  (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: khsx
   :type: Optional[float]


   
   Get or set the Perpendicular coupling, coefficient for effective global X-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: khsy
   :type: Optional[float]


   
   Get or set the Perpendicular coupling, coefficient for effective global Y-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: khsz
   :type: Optional[float]


   
   Get or set the Perpendicular coupling, coefficient for effective global Z-stress (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: hstfac
   :type: float


   
   Get or set the Perpendicular coupling, factor on elastic stiffness (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: hhyper
   :type: Optional[float]


   
   Get or set the Perpendicular coupling, hyperbolic curve limit (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: hlc
   :type: Optional[int]


   
   Get or set the Perpendicular coupling, load curve ID for dimensionless factor on stress as a function of displacement .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'SOIL_PILE_SET'






