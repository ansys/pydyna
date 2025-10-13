





:class:`LoadThermalVariableShell`
=================================


.. py:class:: load_thermal_variable_shell.LoadThermalVariableShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_THERMAL_VARIABLE_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadThermalVariableShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the load ID.
          * - :py:attr:`~eid`
            - Get or set the Shell ID
          * - :py:attr:`~tbase`
            - Get or set the Base temperature
          * - :py:attr:`~tscale`
            - Get or set the Scale factor on temperature from load curve
          * - :py:attr:`~tcurve`
            - Get or set the Load curve ID for temperature vs time
          * - :py:attr:`~tcurdr`
            - Get or set the Load curve ID used during dynamic relaxation
          * - :py:attr:`~zco`
            - Get or set the Relative coordinate through-thickness (-1.0 to +1.0)


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

    from load_thermal_variable_shell import LoadThermalVariableShell

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the load ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Shell ID
















   ..
       !! processed by numpydoc !!

.. py:property:: tbase
   :type: Optional[float]


   
   Get or set the Base temperature
















   ..
       !! processed by numpydoc !!

.. py:property:: tscale
   :type: Optional[float]


   
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

.. py:property:: zco
   :type: Optional[float]


   
   Get or set the Relative coordinate through-thickness (-1.0 to +1.0)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'THERMAL_VARIABLE_SHELL'






