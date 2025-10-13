





:class:`BoundarySymmetryFailure`
================================


.. py:class:: boundary_symmetry_failure.BoundarySymmetryFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SYMMETRY_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySymmetryFailure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid`
            - Get or set the Segment set ID, see *SET_SEGMENT.
          * - :py:attr:`~fs`
            - Get or set the Tensile failure stress FS > 0.0. The average stress in the elements surrounding the boundary nodes in a direction perpendicular to the boundary is used.
          * - :py:attr:`~vtx`
            - Get or set the x-coordinate of tail of a normal vector originating on the wall (tail) and terminating in the body (head), i.e., vector points from the symmetry plane into the body.
          * - :py:attr:`~vty`
            - Get or set the y-coordinate of tail.
          * - :py:attr:`~vtz`
            - Get or set the z-coordinate of tail.
          * - :py:attr:`~vhx`
            - Get or set the x-coordinate of head.
          * - :py:attr:`~vhy`
            - Get or set the y-coordinate of head.
          * - :py:attr:`~vhz`
            - Get or set the z-coordinate of head.


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

    from boundary_symmetry_failure import BoundarySymmetryFailure

Property detail
---------------

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set ID, see *SET_SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Tensile failure stress FS > 0.0. The average stress in the elements surrounding the boundary nodes in a direction perpendicular to the boundary is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtx
   :type: float


   
   Get or set the x-coordinate of tail of a normal vector originating on the wall (tail) and terminating in the body (head), i.e., vector points from the symmetry plane into the body.
















   ..
       !! processed by numpydoc !!

.. py:property:: vty
   :type: float


   
   Get or set the y-coordinate of tail.
















   ..
       !! processed by numpydoc !!

.. py:property:: vtz
   :type: float


   
   Get or set the z-coordinate of tail.
















   ..
       !! processed by numpydoc !!

.. py:property:: vhx
   :type: float


   
   Get or set the x-coordinate of head.
















   ..
       !! processed by numpydoc !!

.. py:property:: vhy
   :type: float


   
   Get or set the y-coordinate of head.
















   ..
       !! processed by numpydoc !!

.. py:property:: vhz
   :type: float


   
   Get or set the z-coordinate of head.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SYMMETRY_FAILURE'






