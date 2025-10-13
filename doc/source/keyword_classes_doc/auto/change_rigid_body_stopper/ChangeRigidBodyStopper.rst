





:class:`ChangeRigidBodyStopper`
===============================


.. py:class:: change_rigid_body_stopper.ChangeRigidBodyStopper(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHANGE_RIGID_BODY_STOPPER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChangeRigidBodyStopper

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of lead rigid body, see *PART.
          * - :py:attr:`~lcmax`
            - Get or set the Load curve ID defining the maximum coordinate as a function of time:
          * - :py:attr:`~lcmin`
            - Get or set the Load curve ID defining the minimum coordinate as a function of time:
          * - :py:attr:`~psidmx`
            - Get or set the Optional part set ID of rigid bodies that are constraned in the maximum coordinate direction to the lead rigid body. This option requires additional input by the *SET_PART definition.
          * - :py:attr:`~psidmn`
            - Get or set the Optional part set ID of rigid bodies that are constraned in the minimum coordinate direction to the lead rigid body. This option requires additional input by the *SET_PART definition.
          * - :py:attr:`~lcvmnx`
            - Get or set the Load curve ID which defines the maximum absolute value of the velocity that is allowed within the stopper:
          * - :py:attr:`~dir`
            - Get or set the Direction stopper acts in:
          * - :py:attr:`~vid`
            - Get or set the Vector for arbitrary orientation of stopper. The vector must be defined by a *DEFINE_VECTOR within the present restart deck.
          * - :py:attr:`~birth`
            - Get or set the Time at which stopper is activated (default = 0.0).
          * - :py:attr:`~death`
            - Get or set the Time at which stopper is deactivated (default = 1.0E+28).


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

    from change_rigid_body_stopper import ChangeRigidBodyStopper

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of lead rigid body, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmax
   :type: int


   
   Get or set the Load curve ID defining the maximum coordinate as a function of time:
   EQ.0: no limitation of the maximum displacement. New curves can be defined by the *DEFINE_CURVE within the present restart deck (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: lcmin
   :type: int


   
   Get or set the Load curve ID defining the minimum coordinate as a function of time:
   EQ.0: no limitation of the minimum displacement. New curves can be defined by the *DEFINE_CURVE within the present restart deck (default).
















   ..
       !! processed by numpydoc !!

.. py:property:: psidmx
   :type: int


   
   Get or set the Optional part set ID of rigid bodies that are constraned in the maximum coordinate direction to the lead rigid body. This option requires additional input by the *SET_PART definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: psidmn
   :type: int


   
   Get or set the Optional part set ID of rigid bodies that are constraned in the minimum coordinate direction to the lead rigid body. This option requires additional input by the *SET_PART definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvmnx
   :type: int


   
   Get or set the Load curve ID which defines the maximum absolute value of the velocity that is allowed within the stopper:
   EQ.0: no limitation of the maximum velocity(default).
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction stopper acts in:
   EQ.1: x-translation,
   EQ.2: y-translation,
   EQ.3: z-translation,
   EQ.4: arbitrary, defined by vector VID,
   EQ.5: x-axis rotation,
   EQ.6: y-axis rotation,
   EQ.7: z-axis rotation,
   EQ.8: arbitrary, defined by vector VID.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the Vector for arbitrary orientation of stopper. The vector must be defined by a *DEFINE_VECTOR within the present restart deck.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time at which stopper is activated (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time at which stopper is deactivated (default = 1.0E+28).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHANGE'


.. py:attribute:: subkeyword
   :value: 'RIGID_BODY_STOPPER'






