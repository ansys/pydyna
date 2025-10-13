





:class:`DefineSphInjection`
===========================


.. py:class:: define_sph_injection.DefineSphInjection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPH_INJECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSphInjection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of newly generated SPH elements.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID. Nodes are used for initial injection position for the SPH elements.
          * - :py:attr:`~cid`
            - Get or set the Local coordinate system ID, see *DEFINE_COORDINATE_SYSTEM. X and Y coordinates define the injection plane, Z coordinate defines the normal to the injection plane.
          * - :py:attr:`~vx`
            - Get or set the X-velocity of the inject elements
          * - :py:attr:`~vy`
            - Get or set the Y-velocity of the inject elements
          * - :py:attr:`~vz`
            - Get or set the Z-velocity of the inject elements
          * - :py:attr:`~area`
            - Get or set the The area of initial injection surface. The density of injection flow comes from the material models see *MAT definition.
          * - :py:attr:`~vmag`
            - Get or set the Injected particle velocity multiplier:
          * - :py:attr:`~tbeg`
            - Get or set the Birth time.
          * - :py:attr:`~tend`
            - Get or set the End time.
          * - :py:attr:`~nid`
            - Get or set the An optional node ID. If defined, the center of the injection plane follows the motion of this node.
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

    from define_sph_injection import DefineSphInjection

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of newly generated SPH elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID. Nodes are used for initial injection position for the SPH elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID, see *DEFINE_COORDINATE_SYSTEM. X and Y coordinates define the injection plane, Z coordinate defines the normal to the injection plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the X-velocity of the inject elements
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Y-velocity of the inject elements
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Z-velocity of the inject elements
















   ..
       !! processed by numpydoc !!

.. py:property:: area
   :type: float


   
   Get or set the The area of initial injection surface. The density of injection flow comes from the material models see *MAT definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: vmag
   :type: int


   
   Get or set the Injected particle velocity multiplier:
   GT.0:   The velocity of the injected particles is multiplied by VMAG.
   LT.0 : |VMAG| is a curve ID defining the magnitude of the velocity vector with respect to time, for variable injection speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: float


   
   Get or set the Birth time.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: float


   
   Get or set the End time.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: int


   
   Get or set the An optional node ID. If defined, the center of the injection plane follows the motion of this node.
















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
   :value: 'SPH_INJECTION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





