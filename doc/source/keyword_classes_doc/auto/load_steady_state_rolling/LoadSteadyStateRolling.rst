





:class:`LoadSteadyStateRolling`
===============================


.. py:class:: load_steady_state_rolling.LoadSteadyStateRolling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_STEADY_STATE_ROLLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSteadyStateRolling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Load steady state rolling ID
          * - :py:attr:`~psid`
            - Get or set the Part Set ID
          * - :py:attr:`~n1`
            - Get or set the Node 1 defining rotational axis
          * - :py:attr:`~n2`
            - Get or set the Node 2 defining rotational axis
          * - :py:attr:`~lcd1`
            - Get or set the Load curve defining angular velocity around rotational axis.
          * - :py:attr:`~lcd1r`
            - Get or set the Optional load curve defining angular velocity around rotational axis for dynamic relaxation. LCD1 is used during dynamic relaxation if LCD1R isn’t defined.
          * - :py:attr:`~n3`
            - Get or set the Node 3 defining turning axis
          * - :py:attr:`~n4`
            - Get or set the Node 4 defining turning axis
          * - :py:attr:`~lcd2`
            - Get or set the Load curve defining angular velocity around turning axis.
          * - :py:attr:`~lcd2r`
            - Get or set the Optional load curve defining angular velocity around turning axis for dynamic relaxation. LCD2 is used during dynamic relaxation if LCD2R isn’t defined
          * - :py:attr:`~n5`
            - Get or set the Node 5 defining translational direction
          * - :py:attr:`~n6`
            - Get or set the Node 6 defining translational direction
          * - :py:attr:`~lcd3`
            - Get or set the Load curve defining translational velocity in translational direction.
          * - :py:attr:`~lcd3r`
            - Get or set the Optional load curve defining translational velocity in translational direction. LCD3 is used during dynamic relaxation if LCD3R isn’t defined.


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

    from load_steady_state_rolling import LoadSteadyStateRolling

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Load steady state rolling ID
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part Set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node 1 defining rotational axis
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node 2 defining rotational axis
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd1
   :type: Optional[int]


   
   Get or set the Load curve defining angular velocity around rotational axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd1r
   :type: Optional[int]


   
   Get or set the Optional load curve defining angular velocity around rotational axis for dynamic relaxation. LCD1 is used during dynamic relaxation if LCD1R isn’t defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Node 3 defining turning axis
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Node 4 defining turning axis
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd2
   :type: Optional[int]


   
   Get or set the Load curve defining angular velocity around turning axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd2r
   :type: Optional[int]


   
   Get or set the Optional load curve defining angular velocity around turning axis for dynamic relaxation. LCD2 is used during dynamic relaxation if LCD2R isn’t defined
















   ..
       !! processed by numpydoc !!

.. py:property:: n5
   :type: Optional[int]


   
   Get or set the Node 5 defining translational direction
















   ..
       !! processed by numpydoc !!

.. py:property:: n6
   :type: Optional[int]


   
   Get or set the Node 6 defining translational direction
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd3
   :type: Optional[int]


   
   Get or set the Load curve defining translational velocity in translational direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcd3r
   :type: Optional[int]


   
   Get or set the Optional load curve defining translational velocity in translational direction. LCD3 is used during dynamic relaxation if LCD3R isn’t defined.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'STEADY_STATE_ROLLING'






