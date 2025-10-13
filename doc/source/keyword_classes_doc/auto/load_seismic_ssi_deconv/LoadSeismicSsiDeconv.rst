





:class:`LoadSeismicSsiDeconv`
=============================


.. py:class:: load_seismic_ssi_deconv.LoadSeismicSsiDeconv(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEISMIC_SSI_DECONV keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSeismicSsiDeconv

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Soil-structure interface ID.
          * - :py:attr:`~xp`
            - Get or set the Curve multiplier at node N1.
          * - :py:attr:`~yp`
            - Get or set the Curve multiplier at node N2.
          * - :py:attr:`~zp`
            - Get or set the Curve multiplier at node N3.
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
          * - :py:attr:`~pset`
            - Get or set the Soil part set through which ground motion travels (DECONV option only)
          * - :py:attr:`~vdir`
            - Get or set the Vertical direction (local if CID ≠ 0) for ground motion propagation (DECONV option only):


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

    from load_seismic_ssi_deconv import LoadSeismicSsiDeconv

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Soil-structure interface ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: float


   
   Get or set the Curve multiplier at node N1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: float


   
   Get or set the Curve multiplier at node N2.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: float


   
   Get or set the Curve multiplier at node N3.
















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

.. py:property:: pset
   :type: Optional[int]


   
   Get or set the Soil part set through which ground motion travels (DECONV option only)
















   ..
       !! processed by numpydoc !!

.. py:property:: vdir
   :type: int


   
   Get or set the Vertical direction (local if CID ≠ 0) for ground motion propagation (DECONV option only):
   EQ. - 1:        -x - direction
   EQ. - 2 : -y - direction
   EQ. - 3 : -z - direction
   EQ.1 : x - direction
   EQ.2 : y - direction
   EQ.3 : z - direction
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEISMIC_SSI_DECONV'






