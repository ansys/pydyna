





:class:`ControlFormingStrainRatioSmooth`
========================================


.. py:class:: control_forming_strain_ratio_smooth.ControlFormingStrainRatioSmooth(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_STRAIN_RATIO_SMOOTH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingStrainRatioSmooth

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dt_cycle`
            - Get or set the Flag for output option (time interval or cycle number).
          * - :py:attr:`~weight`
            - Get or set the Coefficient a in equation below


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

    from control_forming_strain_ratio_smooth import ControlFormingStrainRatioSmooth

Property detail
---------------

.. py:property:: dt_cycle
   :type: Optional[float]


   
   Get or set the Flag for output option (time interval or cycle number).
   LT.0:   the absolute value is the time interval between outputs.
   GT.0:   number of cycles between outputs
















   ..
       !! processed by numpydoc !!

.. py:property:: weight
   :type: Optional[float]


   
   Get or set the Coefficient a in equation below
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_STRAIN_RATIO_SMOOTH'






