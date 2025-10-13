





:class:`InitialFatigueDamageRatio`
==================================


.. py:class:: initial_fatigue_damage_ratio.InitialFatigueDamageRatio(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_FATIGUE_DAMAGE_RATIO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialFatigueDamageRatio

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid_sid`
            - Get or set the Part ID or part set ID for which the initial damage ratio is defined.
          * - :py:attr:`~ptyp`
            - Get or set the Type of PID/PSID:
          * - :py:attr:`~dratio`
            - Get or set the Initial damage ratio.


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

    from initial_fatigue_damage_ratio import InitialFatigueDamageRatio

Property detail
---------------

.. py:property:: pid_sid
   :type: Optional[int]


   
   Get or set the Part ID or part set ID for which the initial damage ratio is defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptyp
   :type: int


   
   Get or set the Type of PID/PSID:
   EQ.0: part ID
   EQ.1: part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dratio
   :type: Optional[float]


   
   Get or set the Initial damage ratio.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'FATIGUE_DAMAGE_RATIO'






