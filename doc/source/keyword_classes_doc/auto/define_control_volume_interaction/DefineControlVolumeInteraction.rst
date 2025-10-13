





:class:`DefineControlVolumeInteraction`
=======================================


.. py:class:: define_control_volume_interaction.DefineControlVolumeInteraction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CONTROL_VOLUME_INTERACTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineControlVolumeInteraction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Fluid cavity interaction ID.
          * - :py:attr:`~cvid1`
            - Get or set the First control volume ID
          * - :py:attr:`~cvid2`
            - Get or set the Second control volume ID
          * - :py:attr:`~lcid_`
            - Get or set the Load curve id (*DEFINE_CURVE_FUNCTION). Tables, see *DEFINE_TABLE, and load curves may not share common IDs.
          * - :py:attr:`~area_`
            - Get or set the This is a constant area for the case when a flow area definition is not defined
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

    from define_control_volume_interaction import DefineControlVolumeInteraction

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Fluid cavity interaction ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvid1
   :type: Optional[int]


   
   Get or set the First control volume ID
















   ..
       !! processed by numpydoc !!

.. py:property:: cvid2
   :type: Optional[int]


   
   Get or set the Second control volume ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid_
   :type: Optional[int]


   
   Get or set the Load curve id (*DEFINE_CURVE_FUNCTION). Tables, see *DEFINE_TABLE, and load curves may not share common IDs.
   LS-DYNA allows load curves IDs and table IDs to be used interchangeably.
   A unique number has to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: area_
   :type: Optional[float]


   
   Get or set the This is a constant area for the case when a flow area definition is not defined
















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
   :value: 'CONTROL_VOLUME_INTERACTION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





