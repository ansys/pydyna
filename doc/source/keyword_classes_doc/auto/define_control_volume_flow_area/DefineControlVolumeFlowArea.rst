





:class:`DefineControlVolumeFlowArea`
====================================


.. py:class:: define_control_volume_flow_area.DefineControlVolumeFlowArea(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CONTROL_VOLUME_FLOW_AREA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineControlVolumeFlowArea

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Flow area ID.
          * - :py:attr:`~sid`
            - Get or set the SET ID defining the flow area
          * - :py:attr:`~stype`
            - Get or set the Type of set defining the flow area.
          * - :py:attr:`~pid_`
            - Get or set the PART ID for null shells for visualizing the flow area. It defaults to 0, in which case the area will not be visualized.
          * - :py:attr:`~area_`
            - Get or set the This is a constant area for the case when a flow area definition is not defined
          * - :py:attr:`~cviid_`
            - Get or set the CONTROL_VOLUME_INTERACTION ID that uses the flow area
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

    from define_control_volume_flow_area import DefineControlVolumeFlowArea

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Flow area ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the SET ID defining the flow area
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Type of set defining the flow area.
   A value of 1 indicates a node set for the perimeter which will be automatically mesh,
   and 2 indicates a segment set covering the flow area
















   ..
       !! processed by numpydoc !!

.. py:property:: pid_
   :type: int


   
   Get or set the PART ID for null shells for visualizing the flow area. It defaults to 0, in which case the area will not be visualized.
















   ..
       !! processed by numpydoc !!

.. py:property:: area_
   :type: Optional[float]


   
   Get or set the This is a constant area for the case when a flow area definition is not defined
















   ..
       !! processed by numpydoc !!

.. py:property:: cviid_
   :type: Optional[float]


   
   Get or set the CONTROL_VOLUME_INTERACTION ID that uses the flow area
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CONTROL_VOLUME_FLOW_AREA'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





