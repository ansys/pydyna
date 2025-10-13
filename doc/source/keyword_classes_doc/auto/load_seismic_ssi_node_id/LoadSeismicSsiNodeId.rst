





:class:`LoadSeismicSsiNodeId`
=============================


.. py:class:: load_seismic_ssi_node_id.LoadSeismicSsiNodeId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEISMIC_SSI_NODE_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSeismicSsiNodeId

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the loading ID
          * - :py:attr:`~heading`
            - Get or set the A description of the loading.
          * - :py:attr:`~ssid`
            - Get or set the Soil-structure interface ID.
          * - :py:attr:`~typeid`
            - Get or set the Node ID (NID in *NODE).
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

    from load_seismic_ssi_node_id import LoadSeismicSsiNodeId

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the A description of the loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Soil-structure interface ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: typeid
   :type: Optional[int]


   
   Get or set the Node ID (NID in *NODE).
















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
   EQ.0: SSID is the ID for the soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.For the DECONV keyword option, ISG = 0 additionally flags that the free-field within motion is computed at depth
   EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure.For the DECONV, ISG = 1 additionally flags that the free-field outcrop motion is computed at depth.
















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
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEISMIC_SSI_NODE_ID'






