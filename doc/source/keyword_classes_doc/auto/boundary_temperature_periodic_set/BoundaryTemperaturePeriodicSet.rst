





:class:`BoundaryTemperaturePeriodicSet`
=======================================


.. py:class:: boundary_temperature_periodic_set.BoundaryTemperaturePeriodicSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_TEMPERATURE_PERIODIC_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryTemperaturePeriodicSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ssid1`
            - Get or set the First Segment set on which the periodic temperature boundary condition will be applied.
          * - :py:attr:`~ptype`
            - Get or set the Type of periodic boundary condition:
          * - :py:attr:`~ssid2`
            - Get or set the Second Segment set on which the periodic temperature boundary condition will be applied.
          * - :py:attr:`~tdlcid`
            - Get or set the Optional load curve specifying the temperature drop, T_drop, between the two surfaces in the periodic boundary condition as a function of time. Note that T_drop =T_1-T_2 where T_1 is the temperature of the surface specified with SSID1 and T_2 is the temperature of the surface specified with SSID2.
          * - :py:attr:`~axe`
            - Get or set the Axis for Ptype=1 or 2 EQ.1:      X-axis
          * - :py:attr:`~nid`
            - Get or set the Node ID giving the origin point coordinates
          * - :py:attr:`~angle`
            - Get or set the Rotation angle if PTYPE=1. Scaling factor on contact distance search if PTYPE=3 (default applies a


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

    from boundary_temperature_periodic_set import BoundaryTemperaturePeriodicSet

Property detail
---------------

.. py:property:: ssid1
   :type: Optional[int]


   
   Get or set the First Segment set on which the periodic temperature boundary condition will be applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: Optional[int]


   
   Get or set the Type of periodic boundary condition:
   EQ.1:   Rotation boundary condition defined by an axis, an origin pointand a rotation angle.
   EQ.2 : Reflective boundary condition defined by an axis and origin point.
   EQ.3 : Sliding boundary condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid2
   :type: Optional[int]


   
   Get or set the Second Segment set on which the periodic temperature boundary condition will be applied.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdlcid
   :type: Optional[int]


   
   Get or set the Optional load curve specifying the temperature drop, T_drop, between the two surfaces in the periodic boundary condition as a function of time. Note that T_drop =T_1-T_2 where T_1 is the temperature of the surface specified with SSID1 and T_2 is the temperature of the surface specified with SSID2.
   EQ.0:   No temperature drop between that surfacs, that is, T_drop = 0.0
















   ..
       !! processed by numpydoc !!

.. py:property:: axe
   :type: Optional[int]


   
   Get or set the Axis for Ptype=1 or 2 EQ.1:      X-axis
   EQ.2:   Y - axis
   EQ.3 : Z - axis.
   Flag for meaning of ANGLE for PTYPE = 3. Setting AXE = 1 means that ANGLE is the contact distance. Otherwise, it is a scale factor on the contact distance search
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID giving the origin point coordinates
















   ..
       !! processed by numpydoc !!

.. py:property:: angle
   :type: Optional[float]


   
   Get or set the Rotation angle if PTYPE=1. Scaling factor on contact distance search if PTYPE=3 (default applies a
   scale factor of 0.3 on local element size). If AXE=1 and PTYPE=3, then ANGLE becomes the contact distance
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'TEMPERATURE_PERIODIC_SET'






