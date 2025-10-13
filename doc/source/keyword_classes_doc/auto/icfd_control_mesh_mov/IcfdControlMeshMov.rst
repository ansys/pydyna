





:class:`IcfdControlMeshMov`
===========================


.. py:class:: icfd_control_mesh_mov.IcfdControlMeshMov(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_MESH_MOV keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlMeshMov

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mmsh`
            - Get or set the Mesh motion selector:
          * - :py:attr:`~lim_iter`
            - Get or set the Maximum number of linear solver iterations for the ball-vertex linear system.
          * - :py:attr:`~reltol`
            - Get or set the Relative tolerance to use as a stopping criterion for the ball-vertex method iterative linear solver (conjugate gradient solver with diagonal scaling preconditioner).


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

    from icfd_control_mesh_mov import IcfdControlMeshMov

Property detail
---------------

.. py:property:: mmsh
   :type: int


   
   Get or set the Mesh motion selector:
   EQ.-1: Completely shuts off any mesh movement
   EQ. 1: mesh moves based on the distance to moving walls.
   EQ. 2: mesh moves by solving a linear elasticity problem using the element sizes as stiffness.
   EQ.3: mesh uses a Laplacian smoothing with stiffness on edges and from node to opposite faces. Very robust but costly.
   EQ 4: full lagrangian. The mesh moves with the velocity of the flow.
   EQ.11: mesh moves using an implicit ball-vertex spring method
   EQ.20 : mesh moves by solving a linear elasticity problem using a constant size. This can be useful to avoid large distortions in rotating problems that involve large discrepancies in mesh sizes (typically in cases involving boundary layer mesh).
















   ..
       !! processed by numpydoc !!

.. py:property:: lim_iter
   :type: int


   
   Get or set the Maximum number of linear solver iterations for the ball-vertex linear system.
















   ..
       !! processed by numpydoc !!

.. py:property:: reltol
   :type: float


   
   Get or set the Relative tolerance to use as a stopping criterion for the ball-vertex method iterative linear solver (conjugate gradient solver with diagonal scaling preconditioner).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_MESH_MOV'






