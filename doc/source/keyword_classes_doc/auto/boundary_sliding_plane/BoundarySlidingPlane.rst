





:class:`BoundarySlidingPlane`
=============================


.. py:class:: boundary_sliding_plane.BoundarySlidingPlane(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SLIDING_PLANE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySlidingPlane

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid`
            - Get or set the Nodal set ID, see *SET_NODE.
          * - :py:attr:`~vx`
            - Get or set the x-coordinate of vector defining normal or vector.
          * - :py:attr:`~vy`
            - Get or set the y-coordinate of vector defining normal or vector.
          * - :py:attr:`~vz`
            - Get or set the z-coordinate of vector defining normal or vector.
          * - :py:attr:`~copt`
            - Get or set the Option:


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

    from boundary_sliding_plane import BoundarySlidingPlane

Property detail
---------------

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Nodal set ID, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the x-coordinate of vector defining normal or vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the y-coordinate of vector defining normal or vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the z-coordinate of vector defining normal or vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: copt
   :type: int


   
   Get or set the Option:
   EQ.0: node moves on normal plane,
   EQ.1: node moves only in vector direction.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SLIDING_PLANE'






