





:class:`ControlHourglass936`
============================


.. py:class:: control_hourglass_936.ControlHourglass936(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_HOURGLASS_936 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlHourglass936

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
            - Get or set the Hourglass coefficient, QH (default = 0.3). Values of QH that exceed .15 may cause instabilities.


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

    from control_hourglass_936 import ControlHourglass936

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
   EQ:6: Belytschko-Bindeman assumed strain co-rotational stiffness form for 2D and 3D solid elements only. Mandatory for implicit anlysis.
















   ..
       !! processed by numpydoc !!

.. py:property:: qh
   :type: float


   
   Get or set the Hourglass coefficient, QH (default = 0.3). Values of QH that exceed .15 may cause instabilities.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'HOURGLASS_936'






