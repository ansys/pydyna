





:class:`DefineContactVolume`
============================


.. py:class:: define_contact_volume.DefineContactVolume(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CONTACT_VOLUME keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineContactVolume

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cvid`
            - Get or set the Contact volume ID.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID. Required for rectangular and cylindrical volumes
          * - :py:attr:`~type`
            - Get or set the Volume type. Set to 0 for rectangular, 1 for cylindrical, and 2 for spherical.
          * - :py:attr:`~xc`
            - Get or set the x-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
          * - :py:attr:`~yc`
            - Get or set the y-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
          * - :py:attr:`~zc`
            - Get or set the z-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
          * - :py:attr:`~xmn`
            - Get or set the Minimum x-coordinate in local coordinate system.
          * - :py:attr:`~xmx`
            - Get or set the Maximum x-coordinate in local coordinate system.
          * - :py:attr:`~ymn`
            - Get or set the Minimum y-coordinate in local coordinate system.
          * - :py:attr:`~ymx`
            - Get or set the Maximum y-coordinate in local coordinate system.
          * - :py:attr:`~zmn`
            - Get or set the Minimum z-coordinate in local coordinate system.
          * - :py:attr:`~zmx`
            - Get or set the Maximum z-coordinate in local coordinate system.
          * - :py:attr:`~length`
            - Get or set the Length of cylinder originating at(xc,yc,zc) and revolving around the local x-axis.
          * - :py:attr:`~rinner`
            - Get or set the Inner radius of cylinder or sphere.
          * - :py:attr:`~router`
            - Get or set the Outer radius of cylinder or sphere.
          * - :py:attr:`~d_angc`
            - Get or set the If the included angle between axis of the cylinder and the normal vector ot the contact segment is less than this angle, the segment is deleted.
          * - :py:attr:`~d_angs`
            - Get or set the If the included angle between a line draw from the center of the sphere to the centroid of the segment, and the normal vector to the contact segment is greater than this angle, the segment is deleted.
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

    from define_contact_volume import DefineContactVolume

Property detail
---------------

.. py:property:: cvid
   :type: Optional[int]


   
   Get or set the Contact volume ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID. Required for rectangular and cylindrical volumes
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Volume type. Set to 0 for rectangular, 1 for cylindrical, and 2 for spherical.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc
   :type: Optional[float]


   
   Get or set the x-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc
   :type: Optional[float]


   
   Get or set the y-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc
   :type: Optional[float]


   
   Get or set the z-coordinate which defines the origin of coordinate system or the center of the sphere for type=3 referenced to the global coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmn
   :type: float


   
   Get or set the Minimum x-coordinate in local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmx
   :type: float


   
   Get or set the Maximum x-coordinate in local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymn
   :type: float


   
   Get or set the Minimum y-coordinate in local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymx
   :type: float


   
   Get or set the Maximum y-coordinate in local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmn
   :type: float


   
   Get or set the Minimum z-coordinate in local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmx
   :type: float


   
   Get or set the Maximum z-coordinate in local coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: length
   :type: float


   
   Get or set the Length of cylinder originating at(xc,yc,zc) and revolving around the local x-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: rinner
   :type: float


   
   Get or set the Inner radius of cylinder or sphere.
















   ..
       !! processed by numpydoc !!

.. py:property:: router
   :type: float


   
   Get or set the Outer radius of cylinder or sphere.
















   ..
       !! processed by numpydoc !!

.. py:property:: d_angc
   :type: float


   
   Get or set the If the included angle between axis of the cylinder and the normal vector ot the contact segment is less than this angle, the segment is deleted.
















   ..
       !! processed by numpydoc !!

.. py:property:: d_angs
   :type: float


   
   Get or set the If the included angle between a line draw from the center of the sphere to the centroid of the segment, and the normal vector to the contact segment is greater than this angle, the segment is deleted.
















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
   :value: 'CONTACT_VOLUME'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





