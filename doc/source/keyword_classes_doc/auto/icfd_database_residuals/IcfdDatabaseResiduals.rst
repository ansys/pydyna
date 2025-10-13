





:class:`IcfdDatabaseResiduals`
==============================


.. py:class:: icfd_database_residuals.IcfdDatabaseResiduals(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_DATABASE_RESIDUALS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdDatabaseResiduals

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rlvl`
            - Get or set the Residual output level :


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

    from icfd_database_residuals import IcfdDatabaseResiduals

Property detail
---------------

.. py:property:: rlvl
   :type: int


   
   Get or set the Residual output level :
   EQ.0: No output.
   EQ.1: Only outputs the number of iterations needed for solving the pressure Poisson equation.
   EQ.2: Outputs the number of iterations for the momentum, pressure, mesh movement and temperature equations.
   EQ.3: Also gives the residual for each iteration during the solve of the momentum, pressure, mesh movement and temperature equations.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'DATABASE_RESIDUALS'






