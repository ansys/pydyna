





:class:`LoadThermalVariableBeam`
================================


.. py:class:: load_thermal_variable_beam.LoadThermalVariableBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_VARIABLE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalVariableBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Load case ID.
          * - :py:attr:`~eid`
            - Get or set the Beam ID
          * - :py:attr:`~ipolar`
            - Get or set the GT.0: the coordinates SCOOR and TCOOR are given in polar coordinates     (see notes)
          * - :py:attr:`~tbase`
            - Get or set the Base temperature
          * - :py:attr:`~tscale`
            - Get or set the Scale factor on temperature from load curve
          * - :py:attr:`~tcurve`
            - Get or set the Load curve ID for temperature vs time
          * - :py:attr:`~tcurdr`
            - Get or set the Load curve ID used during dynamic relaxation
          * - :py:attr:`~scoor`
            - Get or set the Relative coordinate in local S-direction (-1.0 to +1.0)
          * - :py:attr:`~tcoor`
            - Get or set the Relative coordinate in local T-direction (-1.0 to +1.0)


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

    from load_thermal_variable_beam import LoadThermalVariableBeam

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Load case ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Beam ID
















   ..
       !! processed by numpydoc !!

.. py:property:: ipolar
   :type: int


   
   Get or set the GT.0: the coordinates SCOOR and TCOOR are given in polar coordinates     (see notes)
















   ..
       !! processed by numpydoc !!

.. py:property:: tbase
   :type: float


   
   Get or set the Base temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: tscale
   :type: float


   
   Get or set the Scale factor on temperature from load curve
















   ..
       !! processed by numpydoc !!

.. py:property:: tcurve
   :type: Optional[int]


   
   Get or set the Load curve ID for temperature vs time
















   ..
       !! processed by numpydoc !!

.. py:property:: tcurdr
   :type: Optional[int]


   
   Get or set the Load curve ID used during dynamic relaxation
















   ..
       !! processed by numpydoc !!

.. py:property:: scoor
   :type: Optional[float]


   
   Get or set the Relative coordinate in local S-direction (-1.0 to +1.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: tcoor
   :type: Optional[float]


   
   Get or set the Relative coordinate in local T-direction (-1.0 to +1.0)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_VARIABLE_BEAM'






