





:class:`AleTankTest`
====================


.. py:class:: ale_tank_test.AleTankTest(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_TANK_TEST keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleTankTest

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mdotlc`
            - Get or set the LCID for mass flow rate as a function of time. This may be obtained directly from the control-volume type input data.
          * - :py:attr:`~tankvol`
            - Get or set the Volume of the tank used in a tank test from which the tank pressure is measured, and the m(t) and Tgas(t) are computed from this tank pressure data.
          * - :py:attr:`~pamb`
            - Get or set the The pressure inside the tank before jetting (usually 1 bar).
          * - :py:attr:`~pfinal`
            - Get or set the The final equilibrated pressure inside the tank from the tank test.
          * - :py:attr:`~machlim`
            - Get or set the A limiting MACH number for the gass at the throat (MACH=1 preferred).
          * - :py:attr:`~velmax`
            - Get or set the Maximum allowable gas velocity across the inflator orifice (not preferred).
          * - :py:attr:`~aorif`
            - Get or set the Total inflator orifice area (optional, only needed if the *SECTION_POINT_SOURCE card is not used).
          * - :py:attr:`~ammgidg`
            - Get or set the The ALE multi-material group ID (AMMGID) of the gas.
          * - :py:attr:`~ammgida`
            - Get or set the The ALE multi-material group ID (AMMGID) of the air
          * - :py:attr:`~numpnt`
            - Get or set the The number of points in m(t) and Tgas(t) curves. If NUMPNT=0, defaults to 50 points.


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

    from ale_tank_test import AleTankTest

Property detail
---------------

.. py:property:: mdotlc
   :type: int


   
   Get or set the LCID for mass flow rate as a function of time. This may be obtained directly from the control-volume type input data.
















   ..
       !! processed by numpydoc !!

.. py:property:: tankvol
   :type: float


   
   Get or set the Volume of the tank used in a tank test from which the tank pressure is measured, and the m(t) and Tgas(t) are computed from this tank pressure data.
















   ..
       !! processed by numpydoc !!

.. py:property:: pamb
   :type: float


   
   Get or set the The pressure inside the tank before jetting (usually 1 bar).
















   ..
       !! processed by numpydoc !!

.. py:property:: pfinal
   :type: float


   
   Get or set the The final equilibrated pressure inside the tank from the tank test.
















   ..
       !! processed by numpydoc !!

.. py:property:: machlim
   :type: float


   
   Get or set the A limiting MACH number for the gass at the throat (MACH=1 preferred).
















   ..
       !! processed by numpydoc !!

.. py:property:: velmax
   :type: float


   
   Get or set the Maximum allowable gas velocity across the inflator orifice (not preferred).
















   ..
       !! processed by numpydoc !!

.. py:property:: aorif
   :type: float


   
   Get or set the Total inflator orifice area (optional, only needed if the *SECTION_POINT_SOURCE card is not used).
















   ..
       !! processed by numpydoc !!

.. py:property:: ammgidg
   :type: int


   
   Get or set the The ALE multi-material group ID (AMMGID) of the gas.
















   ..
       !! processed by numpydoc !!

.. py:property:: ammgida
   :type: int


   
   Get or set the The ALE multi-material group ID (AMMGID) of the air
















   ..
       !! processed by numpydoc !!

.. py:property:: numpnt
   :type: int


   
   Get or set the The number of points in m(t) and Tgas(t) curves. If NUMPNT=0, defaults to 50 points.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'TANK_TEST'






