





:class:`BoundaryThermalBulkflowSet`
===================================


.. py:class:: boundary_thermal_bulkflow_set.BoundaryThermalBulkflowSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_THERMAL_BULKFLOW_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryThermalBulkflowSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Beam element set ID.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID for mass flow rate versus time.
          * - :py:attr:`~mdot`
            - Get or set the Mass flow rate.


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

    from boundary_thermal_bulkflow_set import BoundaryThermalBulkflowSet

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Beam element set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID for mass flow rate versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: mdot
   :type: Optional[float]


   
   Get or set the Mass flow rate.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'THERMAL_BULKFLOW_SET'






