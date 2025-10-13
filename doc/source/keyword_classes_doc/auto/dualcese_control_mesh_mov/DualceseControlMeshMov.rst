





:class:`DualceseControlMeshMov`
===============================


.. py:class:: dualcese_control_mesh_mov.DualceseControlMeshMov(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_CONTROL_MESH_MOV keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseControlMeshMov

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for this mesh motion algorithm
          * - :py:attr:`~ialg`
            - Get or set the Mesh motion selector:.
          * - :py:attr:`~ninter`
            - Get or set the Number of linear solver iterations (when using a linear solver specified in IALG). No linear solvers have been implemented at this time, so this field is ignored
          * - :py:attr:`~relerr`
            - Get or set the Relative error for determining convergence when using a linear solver specified in IALG. No linear solvers have been implemented at this time, so this field is ignored
          * - :py:attr:`~mxdispr`
            - Get or set the Maximum displacement relative to element size to use as a criterion for avoiding the full calculation of the motion of the DUALCESE part on a given time step. If the full calculation can be avoided, the elements touching an FSI interface are still morphed, but it is assumed that this approximation will not lead to elements that are overly distorted.


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

    from dualcese_control_mesh_mov import DualceseControlMeshMov

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for this mesh motion algorithm
















   ..
       !! processed by numpydoc !!

.. py:property:: ialg
   :type: int


   
   Get or set the Mesh motion selector:.
   EQ.9 (default) : the IDW scheme
















   ..
       !! processed by numpydoc !!

.. py:property:: ninter
   :type: int


   
   Get or set the Number of linear solver iterations (when using a linear solver specified in IALG). No linear solvers have been implemented at this time, so this field is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: relerr
   :type: float


   
   Get or set the Relative error for determining convergence when using a linear solver specified in IALG. No linear solvers have been implemented at this time, so this field is ignored
















   ..
       !! processed by numpydoc !!

.. py:property:: mxdispr
   :type: float


   
   Get or set the Maximum displacement relative to element size to use as a criterion for avoiding the full calculation of the motion of the DUALCESE part on a given time step. If the full calculation can be avoided, the elements touching an FSI interface are still morphed, but it is assumed that this approximation will not lead to elements that are overly distorted.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_MESH_MOV'






