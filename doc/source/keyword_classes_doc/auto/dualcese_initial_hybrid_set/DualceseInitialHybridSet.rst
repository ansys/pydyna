





:class:`DualceseInitialHybridSet`
=================================


.. py:class:: dualcese_initial_hybrid_set.DualceseInitialHybridSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_INITIAL_HYBRID_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseInitialHybridSet

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
          * - :py:attr:`~ra`
            - Get or set the Mass fraction of the reactant(material) with respect to material 2 (the explosive mixture)
          * - :py:attr:`~uic`
            - Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
          * - :py:attr:`~vic`
            - Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
          * - :py:attr:`~wic`
            - Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
          * - :py:attr:`~rho1`
            - Get or set the Density of material 1
          * - :py:attr:`~rho_a`
            - Get or set the Density of the reactant(material a
          * - :py:attr:`~rho_b`
            - Get or set the Density of the product(material b)
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

    from dualcese_initial_hybrid_set import DualceseInitialHybridSet

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

.. py:property:: ra
   :type: Optional[float]


   
   Get or set the Mass fraction of the reactant(material) with respect to material 2 (the explosive mixture)
















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

.. py:property:: rho1
   :type: Optional[float]


   
   Get or set the Density of material 1
















   ..
       !! processed by numpydoc !!

.. py:property:: rho_a
   :type: Optional[float]


   
   Get or set the Density of the reactant(material a
















   ..
       !! processed by numpydoc !!

.. py:property:: rho_b
   :type: Optional[float]


   
   Get or set the Density of the product(material b)
















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
   :value: 'INITIAL_HYBRID_SET'






