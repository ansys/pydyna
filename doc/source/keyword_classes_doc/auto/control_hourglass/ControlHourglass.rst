





:class:`ControlHourglass`
=========================


.. py:class:: control_hourglass.ControlHourglass(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_HOURGLASS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlHourglass

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ihq`
            - Get or set the Default hourglass viscosity type:
          * - :py:attr:`~qh`
            - Get or set the Hourglass coefficient, QH (default = 0.1). Values of QH that exceed 0.15 may cause instabilities.


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

    from control_hourglass import ControlHourglass

Property detail
---------------

.. py:property:: ihq
   :type: Optional[int]


   
   Get or set the Default hourglass viscosity type:
   EQ.1: standard LS-DYNA,
   EQ.2: Flanagan-Belytschko integration,
   EQ.3: Flanagan-Belytschko with exact volume integration,
   EQ.4: stiffness form of type 2 (Flanagan-Belytschko),
   EQ.5: stiffness form of type 3 (Flanagan-Belytschko),
   EQ:6: Belytschko-Bindeman assumed strain co-rotational stiffness form for 2D and 3D solid elements only. Mandatory for implicit analysis.
   EQ.7: Linear total strain form of type 6 hourglass control.  This form is available for explicit and implicit solution method (See remark 6 below).
   EQ:8 Applicable to the type 16 fully integrated shell element.
   IHQ=8 Activates full projection warping stiffness for shell formulations 16 and -16, and is the default for these shell formulations.  A speed penalty of 25% is common for this option.
   EQ.9: Puso [2000] enhanced assumed strain stiffness form for 3D hexahedral elements.
   EQ.10: Cosserat Point Element (CPE) developed by Jabareen and Rubin [2008].
















   ..
       !! processed by numpydoc !!

.. py:property:: qh
   :type: float


   
   Get or set the Hourglass coefficient, QH (default = 0.1). Values of QH that exceed 0.15 may cause instabilities.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'HOURGLASS'






