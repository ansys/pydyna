





:class:`ControlAccuracy`
========================


.. py:class:: control_accuracy.ControlAccuracy(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ACCURACY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAccuracy

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~osu`
            - Get or set the Global flag for 2nd order objective stress update:
          * - :py:attr:`~inn`
            - Get or set the Invariant node numbering for shell and solid elements:
          * - :py:attr:`~pidosu`
            - Get or set the Part set ID for objective stress updates. If this part set ID is given only those part IDs listed will use the objective stress update; therefore, OSU is ignored.
          * - :py:attr:`~iacc`
            - Get or set the Implicit accuracy flag, turns on some specific accuracy considerations in implicit analysis at an extra CPU cost.
          * - :py:attr:`~exacc`
            - Get or set the Explicit accuracy parameter:


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

    from control_accuracy import ControlAccuracy

Property detail
---------------

.. py:property:: osu
   :type: int


   
   Get or set the Global flag for 2nd order objective stress update:
   EQ.0: off (default)
   EQ.1: on
















   ..
       !! processed by numpydoc !!

.. py:property:: inn
   :type: int


   
   Get or set the Invariant node numbering for shell and solid elements:
   EQ.1: off (default for explicit)
   EQ.2: on for shell and thick shell elements(default for implicit)
   EQ.3: On for solid elements
   EQ.4: On for shell, thick shell and solid elements
   EQ.-2:On for shell elements except triangular shells
   EQ.-4:On for both shell and solid elements except triangular shells
















   ..
       !! processed by numpydoc !!

.. py:property:: pidosu
   :type: Optional[int]


   
   Get or set the Part set ID for objective stress updates. If this part set ID is given only those part IDs listed will use the objective stress update; therefore, OSU is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: iacc
   :type: Optional[int]


   
   Get or set the Implicit accuracy flag, turns on some specific accuracy considerations in implicit analysis at an extra CPU cost.
   EQ.0: Off (default)
   EQ.1: On
   EQ.2:   on (partially also for explicit, for compatibility when switching between implicit and explicit)
















   ..
       !! processed by numpydoc !!

.. py:property:: exacc
   :type: Optional[float]


   
   Get or set the Explicit accuracy parameter:
   EQ.0.0: Off(default)
   GT.0.0 : On(see Remark 5)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ACCURACY'






