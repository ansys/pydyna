





:class:`ControlUnits`
=====================


.. py:class:: control_units.ControlUnits(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_UNITS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlUnits

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~length`
            - Get or set the Length unit.
          * - :py:attr:`~time`
            - Get or set the Time unit.
          * - :py:attr:`~mass`
            - Get or set the Mass unit.
          * - :py:attr:`~temp`
            - Get or set the Temperature unit.


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

    from control_units import ControlUnits

Property detail
---------------

.. py:property:: length
   :type: str


   
   Get or set the Length unit.
   m: meter
   mm: millimeter
   cm: centimeter
   mil: mile
   in: inch
   ft: foot
   yd: yard
















   ..
       !! processed by numpydoc !!

.. py:property:: time
   :type: str


   
   Get or set the Time unit.
   sec: second
   min: minute
   hr: hour
   ms: msec, millisec
   micro_s: microsec.
















   ..
       !! processed by numpydoc !!

.. py:property:: mass
   :type: str


   
   Get or set the Mass unit.
   kg: kilogram
   g: gram
   oz: ounce
   lb: pound
   ton: ton
   ntrc_ton
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: str


   
   Get or set the Temperature unit.
   k: kelvin
   c: celsius
   f: fahrenheit
   r: rankine
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'UNITS'






