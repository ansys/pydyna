





:class:`DualceseInitialTwoPhaseSet`
===================================


.. py:class:: dualcese_initial_two_phase_set.DualceseInitialTwoPhaseSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_INITIAL_TWO-PHASE_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseInitialTwoPhaseSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~esid`
            - Get or set the Element set ID (see *DUALCESE_ELEMENTSET)
          * - :py:attr:`~ifunc`
            - Get or set the Option to define initial conditions using *DEFINE_FUNCTION cards:
          * - :py:attr:`~z1`
            - Get or set the Volume fraction of material 1 (or color function).This is usually a value of 0 or 1. For numerical stability, however, use a very small value instead of zero
          * - :py:attr:`~uic`
            - Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
          * - :py:attr:`~vic`
            - Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
          * - :py:attr:`~wic`
            - Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
          * - :py:attr:`~rho_1`
            - Get or set the Density of fluid  1
          * - :py:attr:`~rho_2`
            - Get or set the Density of fluid  2
          * - :py:attr:`~pic`
            - Get or set the Equilibrium multifluid pressure
          * - :py:attr:`~tic`
            - Get or set the Equilibrium multifluid temperaturee


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

    from dualcese_initial_two_phase_set import DualceseInitialTwoPhaseSet

Property detail
---------------

.. py:property:: esid
   :type: Optional[int]


   
   Get or set the Element set ID (see *DUALCESE_ELEMENTSET)
















   ..
       !! processed by numpydoc !!

.. py:property:: ifunc
   :type: Optional[int]


   
   Get or set the Option to define initial conditions using *DEFINE_FUNCTION cards:
   EQ.0:   Not in use.
   EQ.1:All values for initial velocity, pressure, density, and temperature now refer to *DEFINE_FUNCTION IDs. In these functions, the following parameters are allowed: f(x,y,z), meaning that each variable’s initial profile is a function of position
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: Optional[float]


   
   Get or set the Volume fraction of material 1 (or color function).This is usually a value of 0 or 1. For numerical stability, however, use a very small value instead of zero
















   ..
       !! processed by numpydoc !!

.. py:property:: uic
   :type: Optional[float]


   
   Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: vic
   :type: Optional[float]


   
   Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: wic
   :type: Optional[float]


   
   Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
















   ..
       !! processed by numpydoc !!

.. py:property:: rho_1
   :type: Optional[float]


   
   Get or set the Density of fluid  1
















   ..
       !! processed by numpydoc !!

.. py:property:: rho_2
   :type: Optional[float]


   
   Get or set the Density of fluid  2
















   ..
       !! processed by numpydoc !!

.. py:property:: pic
   :type: Optional[float]


   
   Get or set the Equilibrium multifluid pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: tic
   :type: Optional[float]


   
   Get or set the Equilibrium multifluid temperaturee
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'INITIAL_TWO-PHASE_SET'






