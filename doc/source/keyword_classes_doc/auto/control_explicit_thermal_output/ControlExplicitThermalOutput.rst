





:class:`ControlExplicitThermalOutput`
=====================================


.. py:class:: control_explicit_thermal_output.ControlExplicitThermalOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_EXPLICIT_THERMAL_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlExplicitThermalOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtout`
            - Get or set the Time interval between outputs.
          * - :py:attr:`~dtoutyp`
            - Get or set the Type of DTOUT:
          * - :py:attr:`~setid`
            - Get or set the Set ID.  If SETID = 0, then temperatures and enthalpies are output for the whole model.  See Remark 1.
          * - :py:attr:`~setyp`
            - Get or set the Type of set:


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

    from control_explicit_thermal_output import ControlExplicitThermalOutput

Property detail
---------------

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Time interval between outputs.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtoutyp
   :type: int


   
   Get or set the Type of DTOUT:
   EQ.0:   DTOUT is a constant
   EQ.1 : DTOUT is the ID of * DEFINE_CURVE defining a table of  time as function of DTOUT
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: int


   
   Get or set the Set ID.  If SETID = 0, then temperatures and enthalpies are output for the whole model.  See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: setyp
   :type: int


   
   Get or set the Type of set:
   EQ.1:   solid set(see *SET_SOLID)
   EQ.2 : shell set(see *SET_SHELL)
   EQ.3 : beam set(see * SET_BEAM)
   EQ.4 : thick shell set(see *SET_TSHELL).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'EXPLICIT_THERMAL_OUTPUT'






