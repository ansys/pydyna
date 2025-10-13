





:class:`DatabaseTracer`
=======================


.. py:class:: database_tracer.DatabaseTracer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_TRACER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseTracer

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~time`
            - Get or set the Start time for tracer particle
          * - :py:attr:`~track`
            - Get or set the Tracking option:
          * - :py:attr:`~x`
            - Get or set the Initial x-coordinate
          * - :py:attr:`~y`
            - Get or set the Initial y-coordinate
          * - :py:attr:`~z`
            - Get or set the Initial z-coordinate
          * - :py:attr:`~ammgid`
            - Get or set the The AMMG ID (ALE multi-material group) of the material being tracked in a multi-material ALE element. See Remark 1.
          * - :py:attr:`~nid`
            - Get or set the An optional node ID defining the initial position of a tracer particle.
          * - :py:attr:`~radius`
            - Get or set the Radius is used only for the DE option to indicate whether the tracer


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

    from database_tracer import DatabaseTracer

Property detail
---------------

.. py:property:: time
   :type: float


   
   Get or set the Start time for tracer particle
















   ..
       !! processed by numpydoc !!

.. py:property:: track
   :type: int


   
   Get or set the Tracking option:
   EQ.0: particle follows material,
   EQ.1: particle is fixed in space.
   EQ.2: particle follows the mesh
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the Initial x-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the Initial y-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the Initial z-coordinate
















   ..
       !! processed by numpydoc !!

.. py:property:: ammgid
   :type: Optional[int]


   
   Get or set the The AMMG ID (ALE multi-material group) of the material being tracked in a multi-material ALE element. See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the An optional node ID defining the initial position of a tracer particle.
   If defined, its coordinates will overwrite the X, Y, Z coordinates
   above. This feature is for TRACK = 0 only and can be applied to ALE tracers and DE tracers
















   ..
       !! processed by numpydoc !!

.. py:property:: radius
   :type: float


   
   Get or set the Radius is used only for the DE option to indicate whether the tracer
   follows and monitors a single discrete element or multiple discrete elements.
   GT.0: The tracer takes the average results of all discrete elements located inside a sphere with radius = RADIUS. That sphere stays centered on the DE tracer.
   LT.0: The discrete element closest to the tracer is used. The magnitude of RADIUS in this case is unimportant.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'TRACER'






