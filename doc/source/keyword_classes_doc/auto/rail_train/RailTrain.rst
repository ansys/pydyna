





:class:`RailTrain`
==================


.. py:class:: rail_train.RailTrain(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA RAIL_TRAIN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: RailTrain

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Train ID.
          * - :py:attr:`~nsetid`
            - Get or set the Node set ID containing all nodes that are in contact with rails.
          * - :py:attr:`~finit`
            - Get or set the Estimate of initial vertical force on each wheel (optional)   speeds up the process of initial settling down under gravity loading.
          * - :py:attr:`~trid`
            - Get or set the ID of track for this train, see *RAIL_TRACK.
          * - :py:attr:`~lcur`
            - Get or set the Load curve ID (see *DEFINE_CURVE) containing wheel roughness (distance of wheel surface away from perfect circle) vs. distance travelled. The curve does not repeat with each rotation of the wheel   the last point should be at a greater distance than the train is expected to travel. Default: no wheel roughness.
          * - :py:attr:`~offs`
            - Get or set the Offset distance used to generate different roughness curves for each wheel from the roughness curve LCUR. The curve is offset on the x axis by a different whole number multiple of OFFS for each wheel.
          * - :py:attr:`~vertstf`
            - Get or set the Vertical stiffness of rail contact.
          * - :py:attr:`~latstf`
            - Get or set the Lateral stiffness of rail contact.
          * - :py:attr:`~v2`
            - Get or set the Unused variables - leave blank.
          * - :py:attr:`~v3`
            - Get or set the Unused variables - leave blank.
          * - :py:attr:`~l2`
            - Get or set the Lateral clearance from rail to wheel rim. Lateral force is applied to a wheel only when it has moved more than L2 away from the other rail, i.e. the wheel rims are assumed to be near the inner face of the rail.
          * - :py:attr:`~l3`
            - Get or set the Further lateral distance before full lateral stiffness applies (force-deflection curve follows a parabola up to this point).
          * - :py:attr:`~latdir`
            - Get or set the Determines the lateral direction (relative to the track) in which wheel movement is resisted by flange contact. If two wheels are fixed to an axle, lateral force is generally applied to one or other of the two wheels, depending on the direction of lateral movement.
          * - :py:attr:`~fric`
            - Get or set the Coefficient for additional friction force resisting lateral motion of wheel relative to rail.


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

    from rail_train import RailTrain

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Train ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsetid
   :type: Optional[int]


   
   Get or set the Node set ID containing all nodes that are in contact with rails.
















   ..
       !! processed by numpydoc !!

.. py:property:: finit
   :type: float


   
   Get or set the Estimate of initial vertical force on each wheel (optional)   speeds up the process of initial settling down under gravity loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: trid
   :type: int


   
   Get or set the ID of track for this train, see *RAIL_TRACK.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcur
   :type: Optional[int]


   
   Get or set the Load curve ID (see *DEFINE_CURVE) containing wheel roughness (distance of wheel surface away from perfect circle) vs. distance travelled. The curve does not repeat with each rotation of the wheel   the last point should be at a greater distance than the train is expected to travel. Default: no wheel roughness.
















   ..
       !! processed by numpydoc !!

.. py:property:: offs
   :type: float


   
   Get or set the Offset distance used to generate different roughness curves for each wheel from the roughness curve LCUR. The curve is offset on the x axis by a different whole number multiple of OFFS for each wheel.
















   ..
       !! processed by numpydoc !!

.. py:property:: vertstf
   :type: float


   
   Get or set the Vertical stiffness of rail contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: latstf
   :type: float


   
   Get or set the Lateral stiffness of rail contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: float


   
   Get or set the Unused variables - leave blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: float


   
   Get or set the Unused variables - leave blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: l2
   :type: float


   
   Get or set the Lateral clearance from rail to wheel rim. Lateral force is applied to a wheel only when it has moved more than L2 away from the other rail, i.e. the wheel rims are assumed to be near the inner face of the rail.
















   ..
       !! processed by numpydoc !!

.. py:property:: l3
   :type: float


   
   Get or set the Further lateral distance before full lateral stiffness applies (force-deflection curve follows a parabola up to this point).
















   ..
       !! processed by numpydoc !!

.. py:property:: latdir
   :type: float


   
   Get or set the Determines the lateral direction (relative to the track) in which wheel movement is resisted by flange contact. If two wheels are fixed to an axle, lateral force is generally applied to one or other of the two wheels, depending on the direction of lateral movement.
   EQ.0.0: Wheel flanges run on inside faces of rails
   EQ.1.0 : Wheel flanges run on outside faces of rails
   EQ.2.0 : Wheel flanges on both faces of rails(both wheels resist lateral motion in both directions).
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Coefficient for additional friction force resisting lateral motion of wheel relative to rail.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'RAIL'


.. py:attribute:: subkeyword
   :value: 'TRAIN'






