





:class:`ControlFormingHomeGap`
==============================


.. py:class:: control_forming_home_gap.ControlFormingHomeGap(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_HOME_GAP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingHomeGap

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psidu`
            - Get or set the Part set ID of the tools above the blank (upper tools)
          * - :py:attr:`~psidl`
            - Get or set the Part set ID of the tools below the blank (lower tools)
          * - :py:attr:`~gap`
            - Get or set the Minimum gap allowed between the upper and lower tools
          * - :py:attr:`~mvinc`
            - Get or set the Incremental movement of tools from home position to starting position to check the gap
          * - :py:attr:`~istop`
            - Get or set the How to proceed if the minimum gap found is less than GAP:


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

    from control_forming_home_gap import ControlFormingHomeGap

Property detail
---------------

.. py:property:: psidu
   :type: Optional[int]


   
   Get or set the Part set ID of the tools above the blank (upper tools)
















   ..
       !! processed by numpydoc !!

.. py:property:: psidl
   :type: Optional[int]


   
   Get or set the Part set ID of the tools below the blank (lower tools)
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: Optional[float]


   
   Get or set the Minimum gap allowed between the upper and lower tools
















   ..
       !! processed by numpydoc !!

.. py:property:: mvinc
   :type: Optional[float]


   
   Get or set the Incremental movement of tools from home position to starting position to check the gap
















   ..
       !! processed by numpydoc !!

.. py:property:: istop
   :type: int


   
   Get or set the How to proceed if the minimum gap found is less than GAP:
   EQ.0:   Output a warning message.Job continues
   EQ.1 : Terminate the job.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_HOME_GAP'






