





:class:`ComponentGebodMale`
===========================


.. py:class:: component_gebod_male.ComponentGebodMale(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA COMPONENT_GEBOD_MALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ComponentGebodMale

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~did`
            - Get or set the Dummy ID. A unique number must be specified.
          * - :py:attr:`~units`
            - Get or set the System of units used in the finite element model.
          * - :py:attr:`~size`
            - Get or set the Size of the dummy. This represents a combined height and weight percentile ranging from 0 to 100.
          * - :py:attr:`~vx`
            - Get or set the Initial velocity of the dummy in the global x-direction.
          * - :py:attr:`~vy`
            - Get or set the Initial velocity of the dummy in the global y-direction.
          * - :py:attr:`~vz`
            - Get or set the Initial velocity of the dummy in the global z-direction.
          * - :py:attr:`~gx`
            - Get or set the Global x-component of gravitational acceleration applied to the dummy.
          * - :py:attr:`~gy`
            - Get or set the Global y-component of gravitational acceleration applied to the dummy.
          * - :py:attr:`~gz`
            - Get or set the Global z-component of gravitational acceleration applied to the dummy.


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

    from component_gebod_male import ComponentGebodMale

Property detail
---------------

.. py:property:: did
   :type: Optional[int]


   
   Get or set the Dummy ID. A unique number must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: units
   :type: int


   
   Get or set the System of units used in the finite element model.
   EQ.1: lbf*sec^2/in-inch-sec,
   EQ.2: kg-meter-sec,
   EQ.3: kgf*sec^2/mm-mm-sec,
   EQ.4: metric ton-mm-sec,
   EQ.5: kg-mm-msec.
















   ..
       !! processed by numpydoc !!

.. py:property:: size
   :type: Optional[float]


   
   Get or set the Size of the dummy. This represents a combined height and weight percentile ranging from 0 to 100.
















   ..
       !! processed by numpydoc !!

.. py:property:: vx
   :type: float


   
   Get or set the Initial velocity of the dummy in the global x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vy
   :type: float


   
   Get or set the Initial velocity of the dummy in the global y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: vz
   :type: float


   
   Get or set the Initial velocity of the dummy in the global z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: gx
   :type: float


   
   Get or set the Global x-component of gravitational acceleration applied to the dummy.
















   ..
       !! processed by numpydoc !!

.. py:property:: gy
   :type: float


   
   Get or set the Global y-component of gravitational acceleration applied to the dummy.
















   ..
       !! processed by numpydoc !!

.. py:property:: gz
   :type: float


   
   Get or set the Global z-component of gravitational acceleration applied to the dummy.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'COMPONENT'


.. py:attribute:: subkeyword
   :value: 'GEBOD_MALE'






