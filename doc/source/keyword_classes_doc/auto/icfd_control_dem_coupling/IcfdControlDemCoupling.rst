





:class:`IcfdControlDemCoupling`
===============================


.. py:class:: icfd_control_dem_coupling.IcfdControlDemCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_DEM_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlDemCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ctype`
            - Get or set the Indicates the coupling direction to the solver.
          * - :py:attr:`~bt`
            - Get or set the Birth time for the DEM coupling.
          * - :py:attr:`~dt`
            - Get or set the Death time for the DEM coupling.
          * - :py:attr:`~sf`
            - Get or set the Scale factor applied to the force transmitted by the fluid to the structure.
          * - :py:attr:`~maxvel`
            - Get or set the Maximal fluid velocity that can be used for the calculation of the fluid force passed on to the DEM particle. This is to avoid having spurious velocities in the fluid causing very high and unrealistic forces on the DEM particles which may lead to a crash.
          * - :py:attr:`~dtype`
            - Get or set the Drag calculation type :


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

    from icfd_control_dem_coupling import IcfdControlDemCoupling

Property detail
---------------

.. py:property:: ctype
   :type: int


   
   Get or set the Indicates the coupling direction to the solver.
   EQ.0:two-way coupling between the fluid and the solidEQ.
   EQ.1:one-way coupling:The DEM particles transfer their location to the fluid solver.
   EQ.2:one-way coupling. The fluid solver transfers forces to the DEM particles.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time for the DEM coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time for the DEM coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor applied to the force transmitted by the fluid to the structure.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxvel
   :type: Optional[float]


   
   Get or set the Maximal fluid velocity that can be used for the calculation of the fluid force passed on to the DEM particle. This is to avoid having spurious velocities in the fluid causing very high and unrealistic forces on the DEM particles which may lead to a crash.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtype
   :type: Optional[int]


   
   Get or set the Drag calculation type :
   EQ.0:   Constant C_d value 0.5 scaled by SF.
   EQ.1 : Morrison formula for C_d calculation based on local Reynolds number value scaled by SF.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_DEM_COUPLING'






