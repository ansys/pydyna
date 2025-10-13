





:class:`ConstrainedLocal`
=========================


.. py:class:: constrained_local.ConstrainedLocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_LOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedLocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Optional ID which can be referred to by *SENSOR_CONTROL.
          * - :py:attr:`~heading`
            - Get or set the An optional descriptor that will be written into the d3hsp file and the spcforc file.
          * - :py:attr:`~tc`
            - Get or set the Translational Constraint:
          * - :py:attr:`~rc`
            - Get or set the Rotaional Constraint:
          * - :py:attr:`~dir`
            - Get or set the Direction of normal
          * - :py:attr:`~x`
            - Get or set the Local x-coordinate of a point on the local constraint plane
          * - :py:attr:`~y`
            - Get or set the Local y-coordinate of a point on the local constraint plane
          * - :py:attr:`~z`
            - Get or set the Local z-coordinate of a point on the local constraint plane
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID for orientation of the local coordinate system
          * - :py:attr:`~tol`
            - Get or set the User-defined tolerance in length units. If non-zero, the internal mesh-size dependent tolerance gets replaced by this value.


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

    from constrained_local import ConstrainedLocal

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Optional ID which can be referred to by *SENSOR_CONTROL.
   This ID must be unique and cannot be shared with * BOUNDARY_SPC.
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[int]


   
   Get or set the An optional descriptor that will be written into the d3hsp file and the spcforc file.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: int


   
   Get or set the Translational Constraint:
   EQ.1: constrained x translation,
   EQ.2: constrained y translation
   EQ.3: constrained z translation,
   EQ.4: constrained x and y translation,
   EQ.5: constrained y and z translation,
   EQ.6: constrained z and x translation,
   EQ.7: constrained x,y and z translation.
















   ..
       !! processed by numpydoc !!

.. py:property:: rc
   :type: int


   
   Get or set the Rotaional Constraint:
   EQ.1: constrained x rotation,
   EQ.2: constrained y rotaion
   EQ.3: constrained z rotation,
   EQ.4: constrained x and y rotations,
   EQ.5: constrained y and z rotations,
   EQ.6: constrained z and x rotations,
   EQ.7: constrained x,y and z rotations.
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction of normal
   EQ.1:local x,
   EQ.2: local y,
   EQ.3:local z
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Local x-coordinate of a point on the local constraint plane
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Local y-coordinate of a point on the local constraint plane
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Local z-coordinate of a point on the local constraint plane
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID for orientation of the local coordinate system
















   ..
       !! processed by numpydoc !!

.. py:property:: tol
   :type: float


   
   Get or set the User-defined tolerance in length units. If non-zero, the internal mesh-size dependent tolerance gets replaced by this value.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'LOCAL'






