





:class:`CeseControlMeshMov`
===========================


.. py:class:: cese_control_mesh_mov.CeseControlMeshMov(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_CONTROL_MESH_MOV keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseControlMeshMov

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
            - Get or set the Relative tolerance to use as a stopping criterion for the iterative linear solver (conjugate gradient solver with diagonal scaling preconditioner).
          * - :py:attr:`~abstol`
            - Get or set the Absolute tolerance measure for the size of mesh displacement changes to use as a stopping criterion for the iterative linear solver.


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

    from cese_control_mesh_mov import CeseControlMeshMov

Property detail
---------------

.. py:property:: mmsh
   :type: int


   
   Get or set the Mesh motion selector:
   EQ.1: mesh moves using an implicit ball-vertex spring method.
   EQ.9: the IDW scheme is used to move the mesh.
















   ..
       !! processed by numpydoc !!

.. py:property:: lim_iter
   :type: int


   
   Get or set the Maximum number of linear solver iterations for the ball-vertex linear system.
















   ..
       !! processed by numpydoc !!

.. py:property:: reltol
   :type: float


   
   Get or set the Relative tolerance to use as a stopping criterion for the iterative linear solver (conjugate gradient solver with diagonal scaling preconditioner).
















   ..
       !! processed by numpydoc !!

.. py:property:: abstol
   :type: float


   
   Get or set the Absolute tolerance measure for the size of mesh displacement changes to use as a stopping criterion for the iterative linear solver.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_MESH_MOV'






