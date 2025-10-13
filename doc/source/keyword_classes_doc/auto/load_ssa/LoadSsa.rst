





:class:`LoadSsa`
================


.. py:class:: load_ssa.LoadSsa(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SSA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSsa

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vs`
            - Get or set the Sound speed in fluid.
          * - :py:attr:`~ds`
            - Get or set the Density of fluid.
          * - :py:attr:`~refl`
            - Get or set the Consider reflections from sea floor:
          * - :py:attr:`~zb`
            - Get or set the z-coordinate of sea floor. Define only if REFL=1.
          * - :py:attr:`~zsurf`
            - Get or set the z-coordinate of sea surface.
          * - :py:attr:`~fpsid`
            - Get or set the Part set ID of parts subject to flood control. Use the *PART_SET_COLUMN option where the parameters A1 and A2 must be defined as follows:
          * - :py:attr:`~psid`
            - Get or set the Part set ID of parts defining the wet surface. The elements defining these parts must have their outward normals pointing into the fluid.
          * - :py:attr:`~a`
            - Get or set the Shock pressure parameter.
          * - :py:attr:`~alpha`
            - Get or set the Shock pressure parameter.
          * - :py:attr:`~gamma`
            - Get or set the Time constant parameter.
          * - :py:attr:`~ktheta`
            - Get or set the Time constant parameter.
          * - :py:attr:`~kappa`
            - Get or set the Ratio of specific heat capacities.
          * - :py:attr:`~xs`
            - Get or set the x-coordinate of charge.
          * - :py:attr:`~ys`
            - Get or set the y-coordinate of charge.
          * - :py:attr:`~zs`
            - Get or set the z-coordinate of charge.
          * - :py:attr:`~w`
            - Get or set the Weight of charge.
          * - :py:attr:`~tdely`
            - Get or set the Time delay before charge detonates.
          * - :py:attr:`~rad`
            - Get or set the Charge radius.
          * - :py:attr:`~cz`
            - Get or set the Water depth.


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

    from load_ssa import LoadSsa

Property detail
---------------

.. py:property:: vs
   :type: Optional[float]


   
   Get or set the Sound speed in fluid.
















   ..
       !! processed by numpydoc !!

.. py:property:: ds
   :type: Optional[float]


   
   Get or set the Density of fluid.
















   ..
       !! processed by numpydoc !!

.. py:property:: refl
   :type: float


   
   Get or set the Consider reflections from sea floor:
   EQ.0: off,
   EQ.1: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: zb
   :type: float


   
   Get or set the z-coordinate of sea floor. Define only if REFL=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zsurf
   :type: float


   
   Get or set the z-coordinate of sea surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: fpsid
   :type: int


   
   Get or set the Part set ID of parts subject to flood control. Use the *PART_SET_COLUMN option where the parameters A1 and A2 must be defined as follows:
   Parameter A1: Flooding status:
   EQ.1.0: Fluid on both sides,
   EQ.2.0: Fluid outside, air inside,
   EQ.3.0: Air outside, fluid inside,
   EQ.4.0: Material or part is ignored.
   Parameter A2:
   Tubular outer diameter of beam elements. For shell elements this input must be greater than zero for loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID of parts defining the wet surface. The elements defining these parts must have their outward normals pointing into the fluid.
   EQ.0: all parts are included,
   GT.0: define n part ID's in the *SET_PART_COLUMN keyword.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Shock pressure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Shock pressure parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Time constant parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: ktheta
   :type: Optional[float]


   
   Get or set the Time constant parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: kappa
   :type: Optional[float]


   
   Get or set the Ratio of specific heat capacities.
















   ..
       !! processed by numpydoc !!

.. py:property:: xs
   :type: Optional[float]


   
   Get or set the x-coordinate of charge.
















   ..
       !! processed by numpydoc !!

.. py:property:: ys
   :type: Optional[float]


   
   Get or set the y-coordinate of charge.
















   ..
       !! processed by numpydoc !!

.. py:property:: zs
   :type: Optional[float]


   
   Get or set the z-coordinate of charge.
















   ..
       !! processed by numpydoc !!

.. py:property:: w
   :type: Optional[float]


   
   Get or set the Weight of charge.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdely
   :type: Optional[float]


   
   Get or set the Time delay before charge detonates.
















   ..
       !! processed by numpydoc !!

.. py:property:: rad
   :type: Optional[float]


   
   Get or set the Charge radius.
















   ..
       !! processed by numpydoc !!

.. py:property:: cz
   :type: Optional[float]


   
   Get or set the Water depth.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SSA'






