





:class:`BoundarySpcSymmetryPlane`
=================================


.. py:class:: boundary_spc_symmetry_plane.BoundarySpcSymmetryPlane(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_SPC_SYMMETRY_PLANE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundarySpcSymmetryPlane

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idsp`
            - Get or set the Identification number of the constraint. Must be unique.
          * - :py:attr:`~pid`
            - Get or set the Part ID of the deformable part (sheet metal blank, for example) on which the constraints will be imposed.
          * - :py:attr:`~x`
            - Get or set the Position coordinates on the symmetry plane.
          * - :py:attr:`~y`
            - Get or set the Position coordinates on the symmetry plane.
          * - :py:attr:`~z`
            - Get or set the Position coordinates on the symmetry plane.
          * - :py:attr:`~vx`
            - Get or set the Vector components of the symmetry plane normal.
          * - :py:attr:`~vy`
            - Get or set the Vector components of the symmetry plane normal.
          * - :py:attr:`~vz`
            - Get or set the Vector components of the symmetry plane normal.
          * - :py:attr:`~tol`
            - Get or set the A distance tolerance value within which the nodes on the deformable part will be constrained.For shell elements, the default tolerance is 0.2.


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

    from boundary_spc_symmetry_plane import BoundarySpcSymmetryPlane

Property detail
---------------

.. py:property:: idsp
   :type: Optional[int]


   
   Get or set the Identification number of the constraint. Must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the deformable part (sheet metal blank, for example) on which the constraints will be imposed.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the Position coordinates on the symmetry plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the Position coordinates on the symmetry plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the Position coordinates on the symmetry plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Vector components of the symmetry plane normal.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Vector components of the symmetry plane normal.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Vector components of the symmetry plane normal.
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the A distance tolerance value within which the nodes on the deformable part will be constrained.For shell elements, the default tolerance is 0.2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'SPC_SYMMETRY_PLANE'






