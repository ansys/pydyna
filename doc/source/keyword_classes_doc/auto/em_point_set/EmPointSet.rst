





:class:`EmPointSet`
===================


.. py:class:: em_point_set.EmPointSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_POINT_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmPointSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Point Set ID.
          * - :py:attr:`~pstype`
            - Get or set the Point Set type:
          * - :py:attr:`~vx`
            - Get or set the Constant velocities to be used when PSTYPE = 1
          * - :py:attr:`~vy`
            - Get or set the Constant velocities to be used when PSTYPE = 1
          * - :py:attr:`~vz`
            - Get or set the Constant velocities to be used when PSTYPE = 1
          * - :py:attr:`~pid`
            - Get or set the Point ID.
          * - :py:attr:`~x`
            - Get or set the Point initial coordinates
          * - :py:attr:`~y`
            - Get or set the Point initial coordinates
          * - :py:attr:`~z`
            - Get or set the Point initial coordinates
          * - :py:attr:`~pos`
            - Get or set the Position flag:


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

    from em_point_set import EmPointSet

Property detail
---------------

.. py:property:: psid
   :type: int


   
   Get or set the Point Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pstype
   :type: int


   
   Get or set the Point Set type:
   EQ.0: Fixed points.
   EQ.1: Tracer points using prescribed velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Constant velocities to be used when PSTYPE = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Constant velocities to be used when PSTYPE = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Constant velocities to be used when PSTYPE = 1
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Point ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the Point initial coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the Point initial coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the Point initial coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: pos
   :type: int


   
   Get or set the Position flag:
   EQ.0:The solver determines if the point is inside or outside of the conductors.
   EQ.1: Point outside of the conductors during the entire simulation.The solver does not check; hence a gain in computation time.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'POINT_SET'






