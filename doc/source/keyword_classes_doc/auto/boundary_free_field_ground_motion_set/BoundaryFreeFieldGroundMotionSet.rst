





:class:`BoundaryFreeFieldGroundMotionSet`
=========================================


.. py:class:: boundary_free_field_ground_motion_set.BoundaryFreeFieldGroundMotionSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_FREE_FIELD_GROUND_MOTION_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryFreeFieldGroundMotionSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Soil-structure interface ID.
          * - :py:attr:`~typeid`
            - Get or set the Node ID (NID in *NODE) or nodal set ID (SID in *SET_NODE).
          * - :py:attr:`~gmx`
            - Get or set the Acceleration load curve or ground motion ID for motion in the (local) x-direction.
          * - :py:attr:`~gmy`
            - Get or set the Acceleration load curve or ground motion ID for motion in the (local) y-direction.
          * - :py:attr:`~gmz`
            - Get or set the Acceleration load curve or ground motion ID for motion in the (local) z-direction.
          * - :py:attr:`~sf`
            - Get or set the Ground motion scale factor.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
          * - :py:attr:`~birth`
            - Get or set the Time at which specified ground motion is activated.
          * - :py:attr:`~death`
            - Get or set the Time at which specified ground motion is removed.
          * - :py:attr:`~isg`
            - Get or set the Definition of soil-structure interface:
          * - :py:attr:`~igm`
            - Get or set the Specification of ground motions GMX, GMY, GMZ:


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

    from boundary_free_field_ground_motion_set import BoundaryFreeFieldGroundMotionSet

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Soil-structure interface ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: typeid
   :type: Optional[float]


   
   Get or set the Node ID (NID in *NODE) or nodal set ID (SID in *SET_NODE).
















   ..
       !! processed by numpydoc !!

.. py:property:: gmx
   :type: Optional[int]


   
   Get or set the Acceleration load curve or ground motion ID for motion in the (local) x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmy
   :type: Optional[int]


   
   Get or set the Acceleration load curve or ground motion ID for motion in the (local) y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmz
   :type: Optional[int]


   
   Get or set the Acceleration load curve or ground motion ID for motion in the (local) z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Ground motion scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time at which specified ground motion is activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time at which specified ground motion is removed.
















   ..
       !! processed by numpydoc !!

.. py:property:: isg
   :type: int


   
   Get or set the Definition of soil-structure interface:
   EQ.0: SSID is ID for soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.
   EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: igm
   :type: int


   
   Get or set the Specification of ground motions GMX, GMY, GMZ:
   EQ.0: ground motions are specified as acceleration load curves. See *DEFINE_CURVE
   EQ.1: Both ground accelerations and velocities specified using *DEFINE_GROUND_MOTION
   .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'FREE_FIELD_GROUND_MOTION_SET'






