





:class:`DefineGroundMotion`
===========================


.. py:class:: define_ground_motion.DefineGroundMotion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_GROUND_MOTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineGroundMotion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~gmid`
            - Get or set the Ground motion ID. A unique number has to be defined
          * - :py:attr:`~alcid`
            - Get or set the Load Curve ID of ground acceleration history
          * - :py:attr:`~vlcid`
            - Get or set the Load Curve ID of ground velocity history
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

    from define_ground_motion import DefineGroundMotion

Property detail
---------------

.. py:property:: gmid
   :type: Optional[int]


   
   Get or set the Ground motion ID. A unique number has to be defined
















   ..
       !! processed by numpydoc !!

.. py:property:: alcid
   :type: Optional[int]


   
   Get or set the Load Curve ID of ground acceleration history
















   ..
       !! processed by numpydoc !!

.. py:property:: vlcid
   :type: Optional[int]


   
   Get or set the Load Curve ID of ground velocity history
















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
   :value: 'GROUND_MOTION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





