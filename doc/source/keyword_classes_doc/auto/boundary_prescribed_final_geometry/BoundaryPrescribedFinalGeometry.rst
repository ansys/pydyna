





:class:`BoundaryPrescribedFinalGeometry`
========================================


.. py:class:: boundary_prescribed_final_geometry.BoundaryPrescribedFinalGeometry(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_PRESCRIBED_FINAL_GEOMETRY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryPrescribedFinalGeometry

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bpfgid`
            - Get or set the ID for this set of imposed boundary conditions.
          * - :py:attr:`~lcidf`
            - Get or set the Default load curve ID.  This curve varies between zero and unity
          * - :py:attr:`~deathd`
            - Get or set the Default death time.  At this time the prescribed motion is inactive and the nodal point is allowed to move freely
          * - :py:attr:`~nid`
            - Get or set the Node ID for which the final position is defined.  Nodes defined in this section must also appear under the *NODE input.
          * - :py:attr:`~x`
            - Get or set the x-coordinate of final geometry
          * - :py:attr:`~y`
            - Get or set the y-coordinate of final geometry
          * - :py:attr:`~z`
            - Get or set the z-coordinate of final geometry.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID.  If zero the default curve ID, LCIDF, is used.
          * - :py:attr:`~death`
            - Get or set the Death time.  If zero the default value, DEATHD, is used.


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

    from boundary_prescribed_final_geometry import BoundaryPrescribedFinalGeometry

Property detail
---------------

.. py:property:: bpfgid
   :type: int


   
   Get or set the ID for this set of imposed boundary conditions.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidf
   :type: int


   
   Get or set the Default load curve ID.  This curve varies between zero and unity
















   ..
       !! processed by numpydoc !!

.. py:property:: deathd
   :type: Optional[float]


   
   Get or set the Default death time.  At this time the prescribed motion is inactive and the nodal point is allowed to move freely
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID for which the final position is defined.  Nodes defined in this section must also appear under the *NODE input.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the x-coordinate of final geometry
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the y-coordinate of final geometry
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the z-coordinate of final geometry.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID.  If zero the default curve ID, LCIDF, is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: Optional[float]


   
   Get or set the Death time.  If zero the default value, DEATHD, is used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'PRESCRIBED_FINAL_GEOMETRY'






