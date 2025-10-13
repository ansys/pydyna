





:class:`DeformableToRigidAutomatic`
===================================


.. py:class:: deformable_to_rigid_automatic.DeformableToRigidAutomatic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFORMABLE_TO_RIGID_AUTOMATIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DeformableToRigidAutomatic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~swset`
            - Get or set the Set number for this automatic switch set. Must be unique.
          * - :py:attr:`~code`
            - Get or set the Activation switch code. Defines the test to activate the automatic material switch of the part:
          * - :py:attr:`~time1`
            - Get or set the Switch will not take place before this time (default = 0.0).
          * - :py:attr:`~time2`
            - Get or set the Switch will not take place after this time.
          * - :py:attr:`~time3`
            - Get or set the Delay period. Another TIME 3 Delay period. After this part switch has taken place, another automatic switch will not take place for the duration of the delay period. If set to zero a part switch may take place immediately after this switch.
          * - :py:attr:`~entno`
            - Get or set the Rigid wall/contact surface number for switch codes 1, 2, 3, 4.
          * - :py:attr:`~relsw`
            - Get or set the Related switch set.  The related switch set is another automatic switch set paired to this one so the switches can be activated more than once.
          * - :py:attr:`~paired`
            - Get or set the Specify how the related switch sets are paired (if there are paired switches):
          * - :py:attr:`~nrbf`
            - Get or set the Flag to delete or activate nodal rigid bodies.
          * - :py:attr:`~ncsf`
            - Get or set the Flag to delete or activate nodal constraint set.
          * - :py:attr:`~rwf`
            - Get or set the Flag to delete or activate rigid walls.
          * - :py:attr:`~dtmax`
            - Get or set the Maximum permitted time step size after switch.
          * - :py:attr:`~d2r`
            - Get or set the Number of deformable parts to be switched to rigid plus number of rigid parts for which new merged (lead/constrained) rigid body combinations will be defined.
          * - :py:attr:`~r2d`
            - Get or set the Number of rigid parts to be switched to deformable:
          * - :py:attr:`~offset`
            - Get or set the Optional contact thickness for switch to deformable. For contact, its value should be set to a value greater than the contact thickness offsets to ensure the switching occurs prior to impact. This option applies if and only if CODE is set to 3 or 4.  For CODE=3 all rigid wall options are implemented.  For CODE=4, the implementation works for the contact type CONTACT_AUTOMATIC_ when the options: ONE_WAY_ SURFACE_TO_SURFACE,  NODES_TO_SURFACE, and SUR-FACE_ TO_ SURFACE are specified.
          * - :py:attr:`~pid`
            - Get or set the Part ID of the part which is switched to a rigid material.  When PID is merged to another rigid body by the LRB field, this part is allowed to be rigid before the switch..
          * - :py:attr:`~lrb`
            - Get or set the Part ID of the lead rigid body to which part PID is merged.  If zero, part PID becomes either an independent or lead rigid body..
          * - :py:attr:`~ptype`
            - Get or set the Type of PID:


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

    from deformable_to_rigid_automatic import DeformableToRigidAutomatic

Property detail
---------------

.. py:property:: swset
   :type: Optional[int]


   
   Get or set the Set number for this automatic switch set. Must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: code
   :type: int


   
   Get or set the Activation switch code. Defines the test to activate the automatic material switch of the part:
   EQ.0: switch takes place at time 1,
   EQ.1: switch takes place between time 1 and time 2 if rigid wall force is zero,
   EQ.2: switch takes place between time 1 and time 2 if contact surface force is zero,
   EQ.3: switch takes place between time 1 and time 2 if rigid wall force is nonzer,
   EQ.4: switch takes place between time 1 and time 2 if contact surface force is nonzer.
   EQ 5, switch is turned on/off by *SENSOR_CONTROL_DEF2RIG.  Variables other than those identified above will be ignored when CODE=5.
















   ..
       !! processed by numpydoc !!

.. py:property:: time1
   :type: float


   
   Get or set the Switch will not take place before this time (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: time2
   :type: float


   
   Get or set the Switch will not take place after this time.
   EQ.0.0: Time 2 set to 1.0E+20.
















   ..
       !! processed by numpydoc !!

.. py:property:: time3
   :type: float


   
   Get or set the Delay period. Another TIME 3 Delay period. After this part switch has taken place, another automatic switch will not take place for the duration of the delay period. If set to zero a part switch may take place immediately after this switch.
















   ..
       !! processed by numpydoc !!

.. py:property:: entno
   :type: int


   
   Get or set the Rigid wall/contact surface number for switch codes 1, 2, 3, 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: relsw
   :type: int


   
   Get or set the Related switch set.  The related switch set is another automatic switch set paired to this one so the switches can be activated more than once.
   EQ.0:   No related switch set
















   ..
       !! processed by numpydoc !!

.. py:property:: paired
   :type: int


   
   Get or set the Specify how the related switch sets are paired (if there are paired switches):
   EQ.0:   SWSET is not paired to another switch set.
   EQ.1 : SWSET is paired with switch set RELSWand is the first switch set to be activated.
   EQ. - 1 : SWSET is paired with switch set RELSWand is the second switch to be activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrbf
   :type: int


   
   Get or set the Flag to delete or activate nodal rigid bodies.
   If nodal rigid bodies or generalized, weld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instablilities:
   EQ.0: no change,
   EQ.1: delete,
   EQ.2: activate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncsf
   :type: int


   
   Get or set the Flag to delete or activate nodal constraint set.
   If nodal constraint/spotweld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instablilities:
   EQ.0: no change,
   EQ.1: delete,
   EQ.2: activate.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwf
   :type: int


   
   Get or set the Flag to delete or activate rigid walls.
   EQ.0: no change,
   EQ.1: delete,
   EQ.2: activate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtmax
   :type: float


   
   Get or set the Maximum permitted time step size after switch.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2r
   :type: int


   
   Get or set the Number of deformable parts to be switched to rigid plus number of rigid parts for which new merged (lead/constrained) rigid body combinations will be defined.
   EQ.0:   No parts defined
















   ..
       !! processed by numpydoc !!

.. py:property:: r2d
   :type: int


   
   Get or set the Number of rigid parts to be switched to deformable:
   EQ.0: no parts defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: int


   
   Get or set the Optional contact thickness for switch to deformable. For contact, its value should be set to a value greater than the contact thickness offsets to ensure the switching occurs prior to impact. This option applies if and only if CODE is set to 3 or 4.  For CODE=3 all rigid wall options are implemented.  For CODE=4, the implementation works for the contact type CONTACT_AUTOMATIC_ when the options: ONE_WAY_ SURFACE_TO_SURFACE,  NODES_TO_SURFACE, and SUR-FACE_ TO_ SURFACE are specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the part which is switched to a rigid material.  When PID is merged to another rigid body by the LRB field, this part is allowed to be rigid before the switch..
















   ..
       !! processed by numpydoc !!

.. py:property:: lrb
   :type: Optional[int]


   
   Get or set the Part ID of the lead rigid body to which part PID is merged.  If zero, part PID becomes either an independent or lead rigid body..
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: str


   
   Get or set the Type of PID:
   EQ."PART": PID is a part ID.
   EQ."PSET": PID is a part set ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFORMABLE'


.. py:attribute:: subkeyword
   :value: 'TO_RIGID_AUTOMATIC'






