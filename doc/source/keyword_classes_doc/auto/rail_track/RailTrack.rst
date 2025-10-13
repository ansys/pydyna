





:class:`RailTrack`
==================


.. py:class:: rail_track.RailTrack(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RAIL_TRACK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RailTrack

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Track ID
          * - :py:attr:`~bsetid1`
            - Get or set the Beam set ID for rails 1 and 2 containing all beam elements that make up the rail, see *SET_BEAM.
          * - :py:attr:`~norgn1`
            - Get or set the Reference node at one end of each rail, used as the origin for the roughness curve. The train will move in a direction away from this node.
          * - :py:attr:`~lcur1`
            - Get or set the Loadcurve ID (see *DEFINE_CURVE) defining track roughness (vertical displacement from line of beam elements) of the rail as a function of distance from the reference node NORIGIN. Distance from reference node on x-axis of curve, roughness on y-axis. Default: no roughness.
          * - :py:attr:`~oset1`
            - Get or set the Origin of curve LCUR is shifted by distance OSET from the reference node.
          * - :py:attr:`~sf1`
            - Get or set the Roughness values are scaled by SF. Default: 1.0.
          * - :py:attr:`~ga1`
            - Get or set the Shear stiffness of rail per unit length (used to calculate local rail shear deformation within each beam element). GA = shear modulus x cross-sectional area. Default: local shear deformation is ignored.
          * - :py:attr:`~idir`
            - Get or set the Contact forces are calculated in local directions relative to the plane containing the two rails at the contact point. IDIR determines which side of the plane is �up�, that is, the direction in which the wheel can lift off the rail. �Up� is either c or -c,where c=a�b.  a is the direction along rail 1 heading away from node NORGN1 and b is the vector from rail 1 to rail 2. Both a and b are determined locally.
          * - :py:attr:`~bsetid2`
            - Get or set the Beam set ID for rails 1 and 2 containing all beam elements that make up the rail, see *SET_BEAM.
          * - :py:attr:`~norgn2`
            - Get or set the Reference node at one end of each rail, used as the origin for the roughness curve. The train will move in a direction away from this node.
          * - :py:attr:`~lcur2`
            - Get or set the Loadcurve ID (see *DEFINE_CURVE) defining track roughness (vertical displacement from line of beam elements) of the rail as a function of distance from the reference node NORIGIN. Distance from reference node on x-axis of curve, roughness on y-axis. Default: no roughness.
          * - :py:attr:`~oset2`
            - Get or set the Origin of curve LCUR is shifted by distance OSET from the reference node.
          * - :py:attr:`~sf2`
            - Get or set the Roughness values are scaled by SF. Default: 1.0.
          * - :py:attr:`~ga2`
            - Get or set the Shear stiffness of rail per unit length (used to calculate local rail shear deformation within each beam element). GA = shear modulus x cross-sectional area. Default: local shear deformation is ignored.


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

    from rail_track import RailTrack

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Track ID
















   ..
       !! processed by numpydoc !!

.. py:property:: bsetid1
   :type: Optional[int]


   
   Get or set the Beam set ID for rails 1 and 2 containing all beam elements that make up the rail, see *SET_BEAM.
















   ..
       !! processed by numpydoc !!

.. py:property:: norgn1
   :type: Optional[int]


   
   Get or set the Reference node at one end of each rail, used as the origin for the roughness curve. The train will move in a direction away from this node.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcur1
   :type: Optional[int]


   
   Get or set the Loadcurve ID (see *DEFINE_CURVE) defining track roughness (vertical displacement from line of beam elements) of the rail as a function of distance from the reference node NORIGIN. Distance from reference node on x-axis of curve, roughness on y-axis. Default: no roughness.
















   ..
       !! processed by numpydoc !!

.. py:property:: oset1
   :type: float


   
   Get or set the Origin of curve LCUR is shifted by distance OSET from the reference node.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf1
   :type: float


   
   Get or set the Roughness values are scaled by SF. Default: 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: ga1
   :type: float


   
   Get or set the Shear stiffness of rail per unit length (used to calculate local rail shear deformation within each beam element). GA = shear modulus x cross-sectional area. Default: local shear deformation is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: idir
   :type: int


   
   Get or set the Contact forces are calculated in local directions relative to the plane containing the two rails at the contact point. IDIR determines which side of the plane is �up�, that is, the direction in which the wheel can lift off the rail. �Up� is either c or -c,where c=a�b.  a is the direction along rail 1 heading away from node NORGN1 and b is the vector from rail 1 to rail 2. Both a and b are determined locally.
   EQ.0:   Whichever out of c or -c has a positive global Z component is up(default).
   EQ.1 : -c is up.
   EQ. - 1 : c is up.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsetid2
   :type: Optional[int]


   
   Get or set the Beam set ID for rails 1 and 2 containing all beam elements that make up the rail, see *SET_BEAM.
















   ..
       !! processed by numpydoc !!

.. py:property:: norgn2
   :type: Optional[int]


   
   Get or set the Reference node at one end of each rail, used as the origin for the roughness curve. The train will move in a direction away from this node.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcur2
   :type: Optional[int]


   
   Get or set the Loadcurve ID (see *DEFINE_CURVE) defining track roughness (vertical displacement from line of beam elements) of the rail as a function of distance from the reference node NORIGIN. Distance from reference node on x-axis of curve, roughness on y-axis. Default: no roughness.
















   ..
       !! processed by numpydoc !!

.. py:property:: oset2
   :type: float


   
   Get or set the Origin of curve LCUR is shifted by distance OSET from the reference node.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf2
   :type: float


   
   Get or set the Roughness values are scaled by SF. Default: 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: ga2
   :type: float


   
   Get or set the Shear stiffness of rail per unit length (used to calculate local rail shear deformation within each beam element). GA = shear modulus x cross-sectional area. Default: local shear deformation is ignored.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RAIL'


.. py:attribute:: subkeyword
   :value: 'TRACK'






