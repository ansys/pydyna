





:class:`BoundaryThermalWeld`
============================


.. py:class:: boundary_thermal_weld.BoundaryThermalWeld(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_THERMAL_WELD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryThermalWeld

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID or part set ID to which weld source is applied.
          * - :py:attr:`~ptyp`
            - Get or set the PID type:
          * - :py:attr:`~nid`
            - Get or set the Node ID giving location of weld source.
          * - :py:attr:`~nflag`
            - Get or set the Flag controlling motion of weld source:
          * - :py:attr:`~x0`
            - Get or set the x-coordinate of weld source, which remains fixed in space.
          * - :py:attr:`~y0`
            - Get or set the y-coordinate of weld source, which remains fixed in space.
          * - :py:attr:`~z0`
            - Get or set the z-coordinate of weld source, which remains fixed in space.
          * - :py:attr:`~n2id`
            - Get or set the Second node ID for weld beam aiming direction:
          * - :py:attr:`~a`
            - Get or set the Weld pool width.
          * - :py:attr:`~b`
            - Get or set the Weld pool depth (in beam aiming direction).
          * - :py:attr:`~cf`
            - Get or set the Weld pool forward direction.
          * - :py:attr:`~cr`
            - Get or set the Weld pool rearward direction.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for weld energy input rate vs. time
          * - :py:attr:`~q`
            - Get or set the Curve multiplier for weld energy input rate [energy/time, e.g., Watt]
          * - :py:attr:`~ff`
            - Get or set the Forward distribution fraction.
          * - :py:attr:`~fr`
            - Get or set the Rearward distribution fraction.
          * - :py:attr:`~tx`
            - Get or set the x-coordinate of weld beam direction vector in global coordinates.
          * - :py:attr:`~ty`
            - Get or set the y-coordinate of weld beam direction vector in global coordinates.
          * - :py:attr:`~tz`
            - Get or set the z-coordinate of weld beam direction vector in global coordinates.


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

    from boundary_thermal_weld import BoundaryThermalWeld

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID or part set ID to which weld source is applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptyp
   :type: int


   
   Get or set the PID type:
   EQ.1: PID defines a single part ID (default),
   EQ.2: PID defines a part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: int


   
   Get or set the Node ID giving location of weld source.
   EQ.0: location defined by (X0,Y0,Z0) below (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: nflag
   :type: int


   
   Get or set the Flag controlling motion of weld source:
   EQ.1: source moves with node NID (default),
   EQ.2: source is fixed in space at original position of node NID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x0
   :type: Optional[float]


   
   Get or set the x-coordinate of weld source, which remains fixed in space.
   Ignored if NID above is nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: y0
   :type: Optional[float]


   
   Get or set the y-coordinate of weld source, which remains fixed in space.
   Ignored if NID above is nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: z0
   :type: Optional[float]


   
   Get or set the z-coordinate of weld source, which remains fixed in space.
   Ignored if NID above is nonzero.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2id
   :type: Optional[int]


   
   Get or set the Second node ID for weld beam aiming direction:
   GT. 0: beam is aimed from N2ID to NID, moves with these nodes,
   EQ.-1: beam aiming direction is (tx,ty,tz) input on optional card 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Weld pool width.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Weld pool depth (in beam aiming direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: cf
   :type: Optional[float]


   
   Get or set the Weld pool forward direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr
   :type: Optional[float]


   
   Get or set the Weld pool rearward direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID for weld energy input rate vs. time
   EQ.0: use constant multiplier value Q.
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Curve multiplier for weld energy input rate [energy/time, e.g., Watt]
















   ..
       !! processed by numpydoc !!

.. py:property:: ff
   :type: Optional[float]


   
   Get or set the Forward distribution fraction.
   Note: FF + FR = 2.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: fr
   :type: Optional[float]


   
   Get or set the Rearward distribution fraction.
   Note: FF + FR = 2.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: tx
   :type: Optional[float]


   
   Get or set the x-coordinate of weld beam direction vector in global coordinates.
   Define only if N2ID = -1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ty
   :type: Optional[float]


   
   Get or set the y-coordinate of weld beam direction vector in global coordinates.
   Define only if N2ID = -1.
















   ..
       !! processed by numpydoc !!

.. py:property:: tz
   :type: Optional[float]


   
   Get or set the z-coordinate of weld beam direction vector in global coordinates.
   Define only if N2ID = -1.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'THERMAL_WELD'






