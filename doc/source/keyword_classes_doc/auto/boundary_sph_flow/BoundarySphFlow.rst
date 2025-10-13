





:class:`BoundarySphFlow`
========================


.. py:class:: boundary_sph_flow.BoundarySphFlow(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SPH_FLOW keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySphFlow

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Nodal set ID (NSID), SEE *SET_NODE, or part ID (PID), see *PART.
          * - :py:attr:`~styp`
            - Get or set the Set type:
          * - :py:attr:`~dof`
            - Get or set the Applicable degrees-of-freedom:
          * - :py:attr:`~vad`
            - Get or set the Velocity/Acceleration/Displacement flag applied to SPH elements before activation:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe motion value versus time, see *DEFINECURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor. (default=1.0)
          * - :py:attr:`~death`
            - Get or set the Time imposed motion/constraint is removed.
          * - :py:attr:`~birth`
            - Get or set the Time imposed motion/constraint is activated.
          * - :py:attr:`~nid`
            - Get or set the Node fixed in space which determines the boundary between activated particles and deactivated particles
          * - :py:attr:`~vid`
            - Get or set the Vector ID for defining the orientation of the SPH flow. see *DEFINE_VECTOR


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

    from boundary_sph_flow import BoundarySphFlow

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Nodal set ID (NSID), SEE *SET_NODE, or part ID (PID), see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: styp
   :type: int


   
   Get or set the Set type:
   EQ.1: part set ID, see *SET_PART (default),
   EQ.2: part ID, see *PART,
   EQ.3: node set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Applicable degrees-of-freedom:
   EQ.0: Not valid, please use any of the other available options,
   EQ. 1: x-translational degree-of-freedom,
   EQ. 2: y-translational degree-of-freedom,
   EQ. 3: z-translational degree-of-freedom,
   EQ. 4: translational motion in direction given by the VID. Movement on plane normal to the vector is permitted.
















   ..
       !! processed by numpydoc !!

.. py:property:: vad
   :type: int


   
   Get or set the Velocity/Acceleration/Displacement flag applied to SPH elements before activation:
   EQ. 0: velocity,
   EQ. 1: acceleration,
   EQ. 2: displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe motion value versus time, see *DEFINECURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor. (default=1.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time imposed motion/constraint is removed.
   EQ. 0.0: default set to 1020
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time imposed motion/constraint is activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node fixed in space which determines the boundary between activated particles and deactivated particles
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID for defining the orientation of the SPH flow. see *DEFINE_VECTOR
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SPH_FLOW'






