





:class:`LoadThermalLoadCurve`
=============================


.. py:class:: load_thermal_load_curve.LoadThermalLoadCurve(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_LOAD_CURVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalLoadCurve

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE to define temperature versus time.
          * - :py:attr:`~lciddr`
            - Get or set the An optional load curve ID, see *DEFINE_CURVE, to define temperature versus time during the dynamic relaxation phase.


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

    from load_thermal_load_curve import LoadThermalLoadCurve

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE to define temperature versus time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lciddr
   :type: int


   
   Get or set the An optional load curve ID, see *DEFINE_CURVE, to define temperature versus time during the dynamic relaxation phase.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_LOAD_CURVE'






