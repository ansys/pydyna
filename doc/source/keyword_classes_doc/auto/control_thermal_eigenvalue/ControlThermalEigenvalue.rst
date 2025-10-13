





:class:`ControlThermalEigenvalue`
=================================


.. py:class:: control_thermal_eigenvalue.ControlThermalEigenvalue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_THERMAL_EIGENVALUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlThermalEigenvalue

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~neig`
            - Get or set the Number of eigenvalues to compute:


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

    from control_thermal_eigenvalue import ControlThermalEigenvalue

Property detail
---------------

.. py:property:: neig
   :type: int


   
   Get or set the Number of eigenvalues to compute:
   EQ.0:   No eigenvalues are computed.
   GT.0 : Compute NEIG eigenvalues of each thermal conductance matrix.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'THERMAL_EIGENVALUE'






