





:class:`CeseControlSolver`
==========================


.. py:class:: cese_control_solver.CeseControlSolver(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_CONTROL_SOLVER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseControlSolver

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~icese`
            - Get or set the Sets the framework of the CESE solver:   EQ.0: Fixed Eulerian
          * - :py:attr:`~iflow`
            - Get or set the Sets the compressible flow types:
          * - :py:attr:`~igeom`
            - Get or set the Set the geometric dimension:
          * - :py:attr:`~iframe`
            - Get or set the Choose the frame of reference:


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

    from cese_control_solver import CeseControlSolver

Property detail
---------------

.. py:property:: icese
   :type: int


   
   Get or set the Sets the framework of the CESE solver:   EQ.0: Fixed Eulerian
   EQ. 100: Moving Mesh FSI
   EQ. 200: Immersed boundary FSI.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflow
   :type: int


   
   Get or set the Sets the compressible flow types:
   EQ.0: Viscous flows (laminar)
   EQ.1: Inviscid flows
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: igeom
   :type: int


   
   Get or set the Set the geometric dimension:
   EQ.0:   2D or 3D, it will be decided by the mesh & and the given boundary conditions.
   EQ.2:   two dimension (2D) problem
   EQ.3:   three dimension (3D) problem
   EQ.101  2D axis-symmetric
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: iframe
   :type: int


   
   Get or set the Choose the frame of reference:
   EQ.0: Usual non-moving reference frame (default)
   EQ.1000: Non-inertial rotating reference frame.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_SOLVER'






