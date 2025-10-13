





:class:`DefineRegion`
=====================


.. py:class:: define_region.DefineRegion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_REGION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineRegion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Region ID.
          * - :py:attr:`~title`
            - Get or set the Title for this region.
          * - :py:attr:`~type`
            - Get or set the Region type:
          * - :py:attr:`~cid`
            - Get or set the Optional local coordinate system ID. If given, all the following
          * - :py:attr:`~move`
            - Get or set the Flag to specify whether the region moves:
          * - :py:attr:`~xmn`
            - Get or set the Lower x limit of box.
          * - :py:attr:`~xmx`
            - Get or set the Upper x limit of box.
          * - :py:attr:`~ymn`
            - Get or set the Lower y limit of box.
          * - :py:attr:`~ymx`
            - Get or set the Upper y limit of box.
          * - :py:attr:`~zmn`
            - Get or set the Lower z limit of box.
          * - :py:attr:`~zmx`
            - Get or set the Upper z limit of box.


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

    from define_region import DefineRegion

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Region ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Title for this region.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Region type:
   EQ.0: Box
   EQ.1: Sphere or spherical shell
   EQ.2: Cylinder or cylindrical shell, infinite or finite in length
   EQ.3: Ellipsoid.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Optional local coordinate system ID. If given, all the following
   input parameters will be interpreted in this coordinate system.
















   ..
       !! processed by numpydoc !!

.. py:property:: move
   :type: int


   
   Get or set the Flag to specify whether the region moves:
   EQ.0:   Region is stationary.
   EQ.1 : Region moves to follow the local origin and rotates with the local coordinate system(see CID)..
















   ..
       !! processed by numpydoc !!

.. py:property:: xmn
   :type: float


   
   Get or set the Lower x limit of box.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmx
   :type: float


   
   Get or set the Upper x limit of box.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymn
   :type: float


   
   Get or set the Lower y limit of box.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymx
   :type: float


   
   Get or set the Upper y limit of box.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmn
   :type: float


   
   Get or set the Lower z limit of box.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmx
   :type: float


   
   Get or set the Upper z limit of box.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'REGION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





