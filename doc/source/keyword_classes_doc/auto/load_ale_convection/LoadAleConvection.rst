





:class:`LoadAleConvection`
==========================


.. py:class:: load_ale_convection.LoadAleConvection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_ALE_CONVECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadAleConvection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the loading ID
          * - :py:attr:`~heading`
            - Get or set the A description of the loading.
          * - :py:attr:`~lagpid`
            - Get or set the Lagrangian structure PID from a corresponding coupling card which receives the thermal energy in the convection heat transfer
          * - :py:attr:`~lagt`
            - Get or set the Initial temperature of this Lagrangian structure part.
          * - :py:attr:`~lagcp`
            - Get or set the Constant-pressure heat capacity of this Lagrangian structure part.  It has a per-mass unit (for example, J/[kg*K]).
          * - :py:attr:`~h`
            - Get or set the Convection heat transfer coefficient on this Lagrangian structure part surface.  It is the amount of energy (J) transferred per unit area, per time, and per temperature difference
          * - :py:attr:`~lagmas`
            - Get or set the The mass of the Lagrangian structure part receiving the thermal energy.  This is in absolute mass unit


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

    from load_ale_convection import LoadAleConvection

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the A description of the loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: lagpid
   :type: Optional[int]


   
   Get or set the Lagrangian structure PID from a corresponding coupling card which receives the thermal energy in the convection heat transfer
















   ..
       !! processed by numpydoc !!

.. py:property:: lagt
   :type: Optional[float]


   
   Get or set the Initial temperature of this Lagrangian structure part.
















   ..
       !! processed by numpydoc !!

.. py:property:: lagcp
   :type: Optional[float]


   
   Get or set the Constant-pressure heat capacity of this Lagrangian structure part.  It has a per-mass unit (for example, J/[kg*K]).
















   ..
       !! processed by numpydoc !!

.. py:property:: h
   :type: Optional[float]


   
   Get or set the Convection heat transfer coefficient on this Lagrangian structure part surface.  It is the amount of energy (J) transferred per unit area, per time, and per temperature difference
















   ..
       !! processed by numpydoc !!

.. py:property:: lagmas
   :type: Optional[float]


   
   Get or set the The mass of the Lagrangian structure part receiving the thermal energy.  This is in absolute mass unit
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'ALE_CONVECTION'






