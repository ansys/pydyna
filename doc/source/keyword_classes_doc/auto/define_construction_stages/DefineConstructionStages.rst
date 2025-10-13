





:class:`DefineConstructionStages`
=================================


.. py:class:: define_construction_stages.DefineConstructionStages(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CONSTRUCTION_STAGES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineConstructionStages

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~istage`
            - Get or set the Stage ID.
          * - :py:attr:`~ats`
            - Get or set the Analysis time at start of stage
          * - :py:attr:`~ate`
            - Get or set the Analysis time at end of stage.
          * - :py:attr:`~atr`
            - Get or set the Analysis time duration of ramp.
          * - :py:attr:`~rts`
            - Get or set the Real time at start of stage.
          * - :py:attr:`~rte`
            - Get or set the Real time at end of stage.
          * - :py:attr:`~idynain`
            - Get or set the Flag to control output of dynain file at the end of the stage
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

    from define_construction_stages import DefineConstructionStages

Property detail
---------------

.. py:property:: istage
   :type: Optional[int]


   
   Get or set the Stage ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ats
   :type: float


   
   Get or set the Analysis time at start of stage
















   ..
       !! processed by numpydoc !!

.. py:property:: ate
   :type: float


   
   Get or set the Analysis time at end of stage.
















   ..
       !! processed by numpydoc !!

.. py:property:: atr
   :type: Optional[float]


   
   Get or set the Analysis time duration of ramp.
















   ..
       !! processed by numpydoc !!

.. py:property:: rts
   :type: float


   
   Get or set the Real time at start of stage.
















   ..
       !! processed by numpydoc !!

.. py:property:: rte
   :type: float


   
   Get or set the Real time at end of stage.
















   ..
       !! processed by numpydoc !!

.. py:property:: idynain
   :type: int


   
   Get or set the Flag to control output of dynain file at the end of the stage
   EQ.0:   write dynain file
   EQ.1:   do not write dynain file .
















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
   :value: 'CONSTRUCTION_STAGES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





