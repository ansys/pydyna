





:class:`ControlExplicitThermalInitial`
======================================


.. py:class:: control_explicit_thermal_initial.ControlExplicitThermalInitial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_EXPLICIT_THERMAL_INITIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlExplicitThermalInitial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Set ID :
          * - :py:attr:`~idtyp`
            - Get or set the Type of ID:
          * - :py:attr:`~tempini`
            - Get or set the Initial temperature (see Remark 1).


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

    from control_explicit_thermal_initial import ControlExplicitThermalInitial

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Set ID :
   GT.0:   ID is a set
   LT.0 : | ID | is an element.
















   ..
       !! processed by numpydoc !!

.. py:property:: idtyp
   :type: int


   
   Get or set the Type of ID:
   EQ.1: solid
   EQ.2: shell
   EQ.3: beam.
   EQ.4:   thick shell
















   ..
       !! processed by numpydoc !!

.. py:property:: tempini
   :type: float


   
   Get or set the Initial temperature (see Remark 1).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'EXPLICIT_THERMAL_INITIAL'






