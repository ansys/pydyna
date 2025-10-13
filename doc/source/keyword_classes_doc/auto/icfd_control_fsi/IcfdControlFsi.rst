





:class:`IcfdControlFsi`
=======================


.. py:class:: icfd_control_fsi.IcfdControlFsi(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_FSI keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlFsi

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~owc`
            - Get or set the Indicates the coupling direction to the solver.
          * - :py:attr:`~bt`
            - Get or set the Birth time for the FSI coupling. Before BT the fluid solver will not pass any loads to the structure but it will receive displacements from the solid solver.
          * - :py:attr:`~dt`
            - Get or set the Death time for the FSI coupling. After DT the fluid solver will not trans# fer any loads to the solid solver but it will continue to deform with the solid.
          * - :py:attr:`~idc`
            - Get or set the Interaction detection coefficient.
          * - :py:attr:`~lcidsf`
            - Get or set the Optional load curve ID to apply a scaling factor on the forces transferred to the solid :
          * - :py:attr:`~xproj`
            - Get or set the Projection of the nodes of the CFD domain that are at the FSI interface onto the structural mesh.
          * - :py:attr:`~nsub`
            - Get or set the Optional limit on the number of FSI fluid subiterations. This avoids the sometimes unneeded excessive number of FSI subiterations when the fluid and very light structures (like parachutes) develop a resonance-like mode inside the FSI subiterations (coupling iterations)


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

    from icfd_control_fsi import IcfdControlFsi

Property detail
---------------

.. py:property:: owc
   :type: int


   
   Get or set the Indicates the coupling direction to the solver.
   EQ.0:   Two - way coupling.Loads and displacements are transferred across the FSI interface and the full non - linear problem is solved.Weak FSI coupling when coupled to explicit mechanical solver, strong FSI coupling when coupled to implicit mechanical solver.
   EQ.1 : One - way coupling.The solid mechanics solver transfers displacements to the fluid solver.
   EQ.2 : One - way coupling.The fluid solver transfers stresses to the solid mechanics solver.
   EQ.3 : Two - way coupling.Forces weak coupling(no sub - stepping) with implicit mechanical solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time for the FSI coupling. Before BT the fluid solver will not pass any loads to the structure but it will receive displacements from the solid solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time for the FSI coupling. After DT the fluid solver will not trans# fer any loads to the solid solver but it will continue to deform with the solid.
















   ..
       !! processed by numpydoc !!

.. py:property:: idc
   :type: float


   
   Get or set the Interaction detection coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidsf
   :type: Optional[int]


   
   Get or set the Optional load curve ID to apply a scaling factor on the forces transferred to the solid :
   GT.0: Load curve ID function of iterations.
   LT.0: Load curve ID function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: xproj
   :type: int


   
   Get or set the Projection of the nodes of the CFD domain that are at the FSI interface onto the structural mesh.
   EQ.0:No projection
   EQ.1:Projection
















   ..
       !! processed by numpydoc !!

.. py:property:: nsub
   :type: Optional[int]


   
   Get or set the Optional limit on the number of FSI fluid subiterations. This avoids the sometimes unneeded excessive number of FSI subiterations when the fluid and very light structures (like parachutes) develop a resonance-like mode inside the FSI subiterations (coupling iterations)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_FSI'






