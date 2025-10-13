





:class:`DefineDeInjectionEllipse`
=================================


.. py:class:: define_de_injection_ellipse.DefineDeInjectionEllipse(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_INJECTION_ELLIPSE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeInjectionEllipse

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
            - Get or set the Node set ID of new generated DES nodes
          * - :py:attr:`~xc`
            - Get or set the X coordinate of the center of injection plane.
          * - :py:attr:`~yc`
            - Get or set the Y coordinate of the center of injection plane.
          * - :py:attr:`~zc`
            - Get or set the Z coordinate of the center of injection plane.
          * - :py:attr:`~xl`
            - Get or set the Length of the rectangular injection plane along X-axis in the coordinate system(CID) defined.
          * - :py:attr:`~yl`
            - Get or set the Length of the rectangular injection plane along Y-axis in the coordinate system(CID) defined.
          * - :py:attr:`~cid`
            - Get or set the Optional local coordinate system ID.
          * - :py:attr:`~rmass`
            - Get or set the Mass flow rate
          * - :py:attr:`~rmin`
            - Get or set the Minimum DES radius (ignored if IMULTI > 1)
          * - :py:attr:`~rmax`
            - Get or set the Maximum DES radius.(ignored if IMULTI > 1)
          * - :py:attr:`~vx`
            - Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
          * - :py:attr:`~vy`
            - Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
          * - :py:attr:`~vz`
            - Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
          * - :py:attr:`~tbeg`
            - Get or set the Birth time.
          * - :py:attr:`~tend`
            - Get or set the Death time.
          * - :py:attr:`~ifunc`
            - Get or set the Distribution of particle radii (ignored if IMULTI > 1):
          * - :py:attr:`~nid`
            - Get or set the An optional node ID. If defined, the center of injection plane follows the motion of this node
          * - :py:attr:`~imulti`
            - Get or set the Flag for giving a specified mass distribution of injected particles with given radii:
          * - :py:attr:`~lcvx`
            - Get or set the Load curve defines initial injection velocity in x-direction
          * - :py:attr:`~lcvy`
            - Get or set the Load curve defines initial injection velocity in y-direction
          * - :py:attr:`~lcvz`
            - Get or set the Load curve defines initial injection velocity in z-direction
          * - :py:attr:`~r1`
            - Get or set the Injected particle radius.IMULTI radii may be specified
          * - :py:attr:`~p1`
            - Get or set the The mass percentage of injected particle with radius Ri
          * - :py:attr:`~r2`
            - Get or set the Injected particle radius.IMULTI radii may be specified
          * - :py:attr:`~p2`
            - Get or set the The mass percentage of injected particle with radius Ri
          * - :py:attr:`~r3`
            - Get or set the Injected particle radius.IMULTI radii may be specified
          * - :py:attr:`~p3`
            - Get or set the The mass percentage of injected particle with radius Ri
          * - :py:attr:`~r4`
            - Get or set the Injected particle radius.IMULTI radii may be specified
          * - :py:attr:`~p4`
            - Get or set the The mass percentage of injected particle with radius Ri
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

    from define_de_injection_ellipse import DefineDeInjectionEllipse

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of new generated DES nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Node set ID of new generated DES nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: float


   
   Get or set the X coordinate of the center of injection plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: float


   
   Get or set the Y coordinate of the center of injection plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: float


   
   Get or set the Z coordinate of the center of injection plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: xl
   :type: float


   
   Get or set the Length of the rectangular injection plane along X-axis in the coordinate system(CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: yl
   :type: float


   
   Get or set the Length of the rectangular injection plane along Y-axis in the coordinate system(CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Optional local coordinate system ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmass
   :type: Optional[float]


   
   Get or set the Mass flow rate
















   ..
       !! processed by numpydoc !!

.. py:property:: rmin
   :type: Optional[float]


   
   Get or set the Minimum DES radius (ignored if IMULTI > 1)
















   ..
       !! processed by numpydoc !!

.. py:property:: rmax
   :type: Optional[float]


   
   Get or set the Maximum DES radius.(ignored if IMULTI > 1)
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Vector components defining the initial velocity of injected DES in the coordinate system(CID) defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: float


   
   Get or set the Birth time.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: float


   
   Get or set the Death time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifunc
   :type: int


   
   Get or set the Distribution of particle radii (ignored if IMULTI > 1):
   EQ.0: Uniform distribution(Default)
   EQ.1: Gaussian distribution (see Remarks)
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the An optional node ID. If defined, the center of injection plane follows the motion of this node
















   ..
       !! processed by numpydoc !!

.. py:property:: imulti
   :type: Optional[int]


   
   Get or set the Flag for giving a specified mass distribution of injected particles with given radii:
   EQ.1:   Inject the particles with distribution IFUNC using the radii specified with RMINand RMAX(default).
   GT.1 : Inject particles with IMULTI different radii, Ri, with each different size having a specified mass distribution, Pi, given in Card 3.1.IMULTI cannot be greater than 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvx
   :type: Optional[int]


   
   Get or set the Load curve defines initial injection velocity in x-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvy
   :type: Optional[int]


   
   Get or set the Load curve defines initial injection velocity in y-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvz
   :type: Optional[int]


   
   Get or set the Load curve defines initial injection velocity in z-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: float


   
   Get or set the Injected particle radius.IMULTI radii may be specified
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: float


   
   Get or set the The mass percentage of injected particle with radius Ri
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: float


   
   Get or set the Injected particle radius.IMULTI radii may be specified
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: float


   
   Get or set the The mass percentage of injected particle with radius Ri
















   ..
       !! processed by numpydoc !!

.. py:property:: r3
   :type: float


   
   Get or set the Injected particle radius.IMULTI radii may be specified
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: float


   
   Get or set the The mass percentage of injected particle with radius Ri
















   ..
       !! processed by numpydoc !!

.. py:property:: r4
   :type: float


   
   Get or set the Injected particle radius.IMULTI radii may be specified
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: float


   
   Get or set the The mass percentage of injected particle with radius Ri
















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
   :value: 'DE_INJECTION_ELLIPSE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





