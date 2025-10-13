





:class:`DefineDeFlowDrag`
=========================


.. py:class:: define_de_flow_drag.DefineDeFlowDrag(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_FLOW_DRAG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeFlowDrag

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cd`
            - Get or set the Drag coefficient
          * - :py:attr:`~rho`
            - Get or set the Density.
          * - :py:attr:`~mu`
            - Get or set the Dynamic viscosity.
          * - :py:attr:`~vx`
            - Get or set the Velocity.
          * - :py:attr:`~vy`
            - Get or set the Velocity.
          * - :py:attr:`~vz`
            - Get or set the Velocity.
          * - :py:attr:`~tbirth`
            - Get or set the Birth time
          * - :py:attr:`~tdeath`
            - Get or set the Death time
          * - :py:attr:`~vs`
            - Get or set the Sound speed, Vs for CD=-2
          * - :py:attr:`~dflag`
            - Get or set the Influence of neighbors on drag treatment:
          * - :py:attr:`~sfn`
            - Get or set the Scale factors for Cd1_eff respectively when DFLAG=2.
          * - :py:attr:`~sfs`
            - Get or set the Scale factors for Cd2_eff respectively when DFLAG=2.
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

    from define_de_flow_drag import DefineDeFlowDrag

Property detail
---------------

.. py:property:: cd
   :type: float


   
   Get or set the Drag coefficient
   EQ.-1:CD is evaluated based on remark 2.
   EQ.-2: Cd determined based on Allen 2018. See Remark 2. Card 1.1 must be included
















   ..
       !! processed by numpydoc !!

.. py:property:: rho
   :type: float


   
   Get or set the Density.
















   ..
       !! processed by numpydoc !!

.. py:property:: mu
   :type: float


   
   Get or set the Dynamic viscosity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Birth time
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time
















   ..
       !! processed by numpydoc !!

.. py:property:: vs
   :type: float


   
   Get or set the Sound speed, Vs for CD=-2
















   ..
       !! processed by numpydoc !!

.. py:property:: dflag
   :type: int


   
   Get or set the Influence of neighbors on drag treatment:
   EQ.1:   Consider only shadowing effect(default)
   EQ.2 : See Remark 3.
   EQ.3 : See Remark 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfn
   :type: float


   
   Get or set the Scale factors for Cd1_eff respectively when DFLAG=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfs
   :type: float


   
   Get or set the Scale factors for Cd2_eff respectively when DFLAG=2.
















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
   :value: 'DE_FLOW_DRAG'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





