





:class:`DefineDeInjectBondedEllipse`
====================================


.. py:class:: define_de_inject_bonded_ellipse.DefineDeInjectBondedEllipse(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_INJECT_BONDED_ELLIPSE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeInjectBondedEllipse

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of new generated DES nodes
          * - :py:attr:`~sid`
            - Get or set the Node set ID.  Nodes and DES properties are generated automatically during input phase based on the user input and assigned to this SID
          * - :py:attr:`~xc`
            - Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
          * - :py:attr:`~yc`
            - Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
          * - :py:attr:`~zc`
            - Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
          * - :py:attr:`~xl`
            - Get or set the For rectangular planes XL specifies the planar length along the x-axis in the coordinate system specified by CID.  For elliptical planes XL specifies the length of the major axis
          * - :py:attr:`~yl`
            - Get or set the For rectangular planes YL specifies the planar length along the y-axis in the coordinate system specified by CID.  For elliptical planes YL specifies the length of the minor axis
          * - :py:attr:`~cid`
            - Get or set the Optional local coordinate system ID, see *DEFINE_COORDINATE_SYSTEM
          * - :py:attr:`~rmass`
            - Get or set the Mass flow rate
          * - :py:attr:`~vx`
            - Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
          * - :py:attr:`~xy`
            - Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
          * - :py:attr:`~vz`
            - Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
          * - :py:attr:`~tbeg`
            - Get or set the Birth time; time for injection to begin
          * - :py:attr:`~tend`
            - Get or set the Death time; time for injection to end
          * - :py:attr:`~pbn`
            - Get or set the Parallel-bond modulus [Pa].
          * - :py:attr:`~pbs`
            - Get or set the Parallel-bond stiffness ratio.  Shear stiffness/normal stiffness
          * - :py:attr:`~pbn_s`
            - Get or set the Parallel-bond maximum normal stress.  A zero value defines an infinite maximum normal stress
          * - :py:attr:`~pbs_s`
            - Get or set the Parallel-bond maximum shear stress.  A zero value defines an infinite maximum shear stress
          * - :py:attr:`~sfa`
            - Get or set the Bond radius multiplier
          * - :py:attr:`~alpha`
            - Get or set the Numerical damping
          * - :py:attr:`~maxgap`
            - Get or set the Maximum gap between two bonded spheres
          * - :py:attr:`~nshape`
            - Get or set the Number of shape patterns
          * - :py:attr:`~ishape`
            - Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_de_inject_bonded_ellipse import DefineDeInjectBondedEllipse

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of new generated DES nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Node set ID.  Nodes and DES properties are generated automatically during input phase based on the user input and assigned to this SID
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the , 𝑦, 𝑧 coordinates of the center of injection plane
















   ..
       !! processed by numpydoc !!

.. py:property:: xl
   :type: float


   
   Get or set the For rectangular planes XL specifies the planar length along the x-axis in the coordinate system specified by CID.  For elliptical planes XL specifies the length of the major axis
















   ..
       !! processed by numpydoc !!

.. py:property:: yl
   :type: float


   
   Get or set the For rectangular planes YL specifies the planar length along the y-axis in the coordinate system specified by CID.  For elliptical planes YL specifies the length of the minor axis
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Optional local coordinate system ID, see *DEFINE_COORDINATE_SYSTEM
















   ..
       !! processed by numpydoc !!

.. py:property:: rmass
   :type: float


   
   Get or set the Mass flow rate
   GE.0.0: Constant mass flow rate
   LT.0.0 : RMASS is a curve ID defining the mass flow rate as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
















   ..
       !! processed by numpydoc !!

.. py:property:: xy
   :type: float


   
   Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Vector components defining the initial velocity of injected DES specified relative the coordinate system defined by CID
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: float


   
   Get or set the Birth time; time for injection to begin
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: float


   
   Get or set the Death time; time for injection to end
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn
   :type: Optional[float]


   
   Get or set the Parallel-bond modulus [Pa].
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs
   :type: Optional[float]


   
   Get or set the Parallel-bond stiffness ratio.  Shear stiffness/normal stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: pbn_s
   :type: float


   
   Get or set the Parallel-bond maximum normal stress.  A zero value defines an infinite maximum normal stress
















   ..
       !! processed by numpydoc !!

.. py:property:: pbs_s
   :type: float


   
   Get or set the Parallel-bond maximum shear stress.  A zero value defines an infinite maximum shear stress
















   ..
       !! processed by numpydoc !!

.. py:property:: sfa
   :type: float


   
   Get or set the Bond radius multiplier
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the Numerical damping
















   ..
       !! processed by numpydoc !!

.. py:property:: maxgap
   :type: float


   
   Get or set the Maximum gap between two bonded spheres
   GT.0.0: When MAXGAP is positive, the maximum allowed gap is determined on a bond - by - bond basis as a function of the radii of the two involved spheres.The maximum gap is determined by multiplying the minimum of the two radii by the value of MAXGAP.
   LT.0.0 : Absolute value is used as the maximum gap
















   ..
       !! processed by numpydoc !!

.. py:property:: nshape
   :type: int


   
   Get or set the Number of shape patterns
















   ..
       !! processed by numpydoc !!

.. py:property:: ishape
   :type: Optional[int]


   
   Get or set the The pattern ID defined in *DEFINE_DE_INJECT_SHAPE. Only the first NSHAPE number of IDs will be used
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'DE_INJECT_BONDED_ELLIPSE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





