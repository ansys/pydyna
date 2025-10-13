





:class:`MatHydraulicGasDamperDiscreteBeam`
==========================================


.. py:class:: mat_hydraulic_gas_damper_discrete_beam.MatHydraulicGasDamperDiscreteBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_HYDRAULIC_GAS_DAMPER_DISCRETE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatHydraulicGasDamperDiscreteBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density, see also volume in *SECTION_BEAM defintion.
          * - :py:attr:`~co`
            - Get or set the Length of gas column.
          * - :py:attr:`~n`
            - Get or set the Adiabatic constant.
          * - :py:attr:`~p0`
            - Get or set the Initial gas pressure.
          * - :py:attr:`~pa`
            - Get or set the Atmospheric pressure.
          * - :py:attr:`~ap`
            - Get or set the Piston cross sectional area.
          * - :py:attr:`~kh`
            - Get or set the Hydraulic constant, K.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID, see *DEFINE_CURVE, defining the orifice area, versus element deflection.
          * - :py:attr:`~fr`
            - Get or set the Return factor on orifice force. Represents a valve, that opens when the piston unloads to relieve hydraulic pressure. Set to one for no relief.
          * - :py:attr:`~sclf`
            - Get or set the Scale factor on force (default = 1.0).
          * - :py:attr:`~clear`
            - Get or set the Clearance (if nonzero, no tensile force develops for positive displacements and negative forces develop only after the clearance is closed).
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from mat_hydraulic_gas_damper_discrete_beam import MatHydraulicGasDamperDiscreteBeam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density, see also volume in *SECTION_BEAM defintion.
















   ..
       !! processed by numpydoc !!

.. py:property:: co
   :type: Optional[float]


   
   Get or set the Length of gas column.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Adiabatic constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: p0
   :type: Optional[float]


   
   Get or set the Initial gas pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: pa
   :type: Optional[float]


   
   Get or set the Atmospheric pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ap
   :type: Optional[float]


   
   Get or set the Piston cross sectional area.
















   ..
       !! processed by numpydoc !!

.. py:property:: kh
   :type: Optional[float]


   
   Get or set the Hydraulic constant, K.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID, see *DEFINE_CURVE, defining the orifice area, versus element deflection.
















   ..
       !! processed by numpydoc !!

.. py:property:: fr
   :type: Optional[float]


   
   Get or set the Return factor on orifice force. Represents a valve, that opens when the piston unloads to relieve hydraulic pressure. Set to one for no relief.
















   ..
       !! processed by numpydoc !!

.. py:property:: sclf
   :type: float


   
   Get or set the Scale factor on force (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: clear
   :type: Optional[float]


   
   Get or set the Clearance (if nonzero, no tensile force develops for positive displacements and negative forces develop only after the clearance is closed).
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'HYDRAULIC_GAS_DAMPER_DISCRETE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





