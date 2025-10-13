





:class:`ControlFormingTravel`
=============================


.. py:class:: control_forming_travel.ControlFormingTravel(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_TRAVEL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingTravel

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID
          * - :py:attr:`~vid`
            - Get or set the Vector ID defining the tool's movement orientation
          * - :py:attr:`~travel`
            - Get or set the Move PID a certain distance
          * - :py:attr:`~target`
            - Get or set the Move PID to meet TARGET
          * - :py:attr:`~gap`
            - Get or set the The minimum distance between PID and TARGET
          * - :py:attr:`~phase`
            - Get or set the Phase number
          * - :py:attr:`~follow`
            - Get or set the PID can also move by following the part defined in FOLLOW. During this phase, the distance between PID and FOLLOW will Be constant


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

    from control_forming_travel import ControlFormingTravel

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: Optional[int]


   
   Get or set the Vector ID defining the tool's movement orientation
















   ..
       !! processed by numpydoc !!

.. py:property:: travel
   :type: Optional[float]


   
   Get or set the Move PID a certain distance
















   ..
       !! processed by numpydoc !!

.. py:property:: target
   :type: Optional[int]


   
   Get or set the Move PID to meet TARGET
















   ..
       !! processed by numpydoc !!

.. py:property:: gap
   :type: Optional[float]


   
   Get or set the The minimum distance between PID and TARGET
















   ..
       !! processed by numpydoc !!

.. py:property:: phase
   :type: Optional[int]


   
   Get or set the Phase number
















   ..
       !! processed by numpydoc !!

.. py:property:: follow
   :type: Optional[int]


   
   Get or set the PID can also move by following the part defined in FOLLOW. During this phase, the distance between PID and FOLLOW will Be constant
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_TRAVEL'






