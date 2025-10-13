





:class:`ComponentHybridiii`
===========================


.. py:class:: component_hybridiii.ComponentHybridiii(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA COMPONENT_HYBRIDIII keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ComponentHybridiii

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~did`
            - Get or set the Dummy ID. A unique number must be specified.
          * - :py:attr:`~size`
            - Get or set the Size of dummy:
          * - :py:attr:`~units`
            - Get or set the System of units used in the finite element model:
          * - :py:attr:`~defrm`
            - Get or set the Deformability type:
          * - :py:attr:`~vx`
            - Get or set the Initial velocity of the dummy in the global x-direction.
          * - :py:attr:`~vy`
            - Get or set the Initial velocity of the dummy in the global y-direction.
          * - :py:attr:`~vz`
            - Get or set the Initial velocity of the dummy in the global z-direction.
          * - :py:attr:`~hx`
            - Get or set the Initial global x-coordinate value of the H-point.
          * - :py:attr:`~hy`
            - Get or set the Initial global y-coordinate value of the H-point.
          * - :py:attr:`~hz`
            - Get or set the Initial global z-coordinate value of the H-point.
          * - :py:attr:`~rx`
            - Get or set the Initial rotation of dummy about the H-point with respect to the global x-axis (degrees).
          * - :py:attr:`~ry`
            - Get or set the Initial rotation of dummy about the H-point with respect to the global y-axis (degrees).
          * - :py:attr:`~rz`
            - Get or set the Initial rotation of dummy about the H-point with respect to the global z-axis (degrees).


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

    from component_hybridiii import ComponentHybridiii

Property detail
---------------

.. py:property:: did
   :type: Optional[int]


   
   Get or set the Dummy ID. A unique number must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: size
   :type: int


   
   Get or set the Size of dummy:
   EQ.1: 5th percentile adult (default),
   EQ.2: 50th percentile adult,
   EQ.3: 95th percentile adult.
















   ..
       !! processed by numpydoc !!

.. py:property:: units
   :type: int


   
   Get or set the System of units used in the finite element model:
   EQ.1: lbf*sec^2/in-inch-sec (default),
   EQ.2: kg-meter-sec,
   EQ.3:kgf*sec^2/mm-mm-sec,
   EQ.4: metric ton-mm-sec,
   EQ.5: kg-mm-msec.
















   ..
       !! processed by numpydoc !!

.. py:property:: defrm
   :type: int


   
   Get or set the Deformability type:
   EQ.1: all dummy segments entirely rigid (default),
   EQ.2: deformable abdomen (low density foam, mat #57),
   EQ.3: deformable jacket (low density foam, mat #57),
   EQ.4: deformable headskin (viscoelastic, mat #6),
   EQ.5: deformable abdomen/jacket,
   EQ.6: deformable jacket/headskin,
   EQ.7: deformable abdomen/headskin,
   EQ.8: deformable abdomen/jacket/headskin.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Initial velocity of the dummy in the global x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Initial velocity of the dummy in the global y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Initial velocity of the dummy in the global z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: hx
   :type: float


   
   Get or set the Initial global x-coordinate value of the H-point.
















   ..
       !! processed by numpydoc !!

.. py:property:: hy
   :type: float


   
   Get or set the Initial global y-coordinate value of the H-point.
















   ..
       !! processed by numpydoc !!

.. py:property:: hz
   :type: float


   
   Get or set the Initial global z-coordinate value of the H-point.
















   ..
       !! processed by numpydoc !!

.. py:property:: rx
   :type: float


   
   Get or set the Initial rotation of dummy about the H-point with respect to the global x-axis (degrees).
















   ..
       !! processed by numpydoc !!

.. py:property:: ry
   :type: float


   
   Get or set the Initial rotation of dummy about the H-point with respect to the global y-axis (degrees).
















   ..
       !! processed by numpydoc !!

.. py:property:: rz
   :type: float


   
   Get or set the Initial rotation of dummy about the H-point with respect to the global z-axis (degrees).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'COMPONENT'


.. py:attribute:: subkeyword
   :value: 'HYBRIDIII'






