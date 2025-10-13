





:class:`DefineDeInjection`
==========================


.. py:class:: define_de_injection.DefineDeInjection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_INJECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeInjection

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

    from define_de_injection import DefineDeInjection

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
   :value: 'DE_INJECTION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





