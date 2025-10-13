





:class:`SensorCpmAirbag`
========================


.. py:class:: sensor_cpm_airbag.SensorCpmAirbag(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SENSOR_CPM_AIRBAG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SensorCpmAirbag

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cpmid`
            - Get or set the Bag ID of *AIRBAG_PARTICLE_ID.
          * - :py:attr:`~switid`
            - Get or set the Switch ID of *SENSOR_SWITCH.
          * - :py:attr:`~tbirth`
            - Get or set the If SWITID is set, TBIRTH is not active. If SWITID is 0, TBIRTH is
          * - :py:attr:`~tdeath`
            - Get or set the Disable the CPMID bag when the simulation time exceeds this value.
          * - :py:attr:`~tdr`
            - Get or set the If TDR is greater than 0 the bag with ID = CPMID will be rigid
          * - :py:attr:`~defps`
            - Get or set the Part set ID specifiying which parts of the bag with ID = CPMID are deformable.
          * - :py:attr:`~rbpid`
            - Get or set the Part ID of the rigid body to which the part is merged.
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

    from sensor_cpm_airbag import SensorCpmAirbag

Property detail
---------------

.. py:property:: cpmid
   :type: Optional[int]


   
   Get or set the Bag ID of *AIRBAG_PARTICLE_ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: switid
   :type: Optional[int]


   
   Get or set the Switch ID of *SENSOR_SWITCH.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: Optional[float]


   
   Get or set the If SWITID is set, TBIRTH is not active. If SWITID is 0, TBIRTH is
   the activation time for the bag with ID = CPMID. All of the time
   dependent curves that are used in this bag will be offset by the value of TBIRTH.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: Optional[float]


   
   Get or set the Disable the CPMID bag when the simulation time exceeds this value.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdr
   :type: Optional[float]


   
   Get or set the If TDR is greater than 0 the bag with ID = CPMID will be rigid
   starting at first cycle and switch to deformable at time TDR.
















   ..
       !! processed by numpydoc !!

.. py:property:: defps
   :type: Optional[int]


   
   Get or set the Part set ID specifiying which parts of the bag with ID = CPMID are deformable.
















   ..
       !! processed by numpydoc !!

.. py:property:: rbpid
   :type: Optional[int]


   
   Get or set the Part ID of the rigid body to which the part is merged.
















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
   :value: 'SENSOR'


.. py:attribute:: subkeyword
   :value: 'CPM_AIRBAG'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





