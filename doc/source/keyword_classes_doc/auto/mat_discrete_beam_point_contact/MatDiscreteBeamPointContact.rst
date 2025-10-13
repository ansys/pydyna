





:class:`MatDiscreteBeamPointContact`
====================================


.. py:class:: mat_discrete_beam_point_contact.MatDiscreteBeamPointContact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_DISCRETE_BEAM_POINT_CONTACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatDiscreteBeamPointContact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~stiff`
            - Get or set the Stiffness (Force/length units).
          * - :py:attr:`~fric`
            - Get or set the Friction coefficient (dimensionless).
          * - :py:attr:`~damp`
            - Get or set the Damping factor (dimensionless), in the range 0 to 1. Suggested value 0.5.
          * - :py:attr:`~dmxpz`
            - Get or set the Displacement limit in positive local Z direction (uplift).
          * - :py:attr:`~limpz`
            - Get or set the Action when Node 2 passes DMXPZ:
          * - :py:attr:`~dmxpx`
            - Get or set the Displacement limit in positive local X direction.
          * - :py:attr:`~dmxnx`
            - Get or set the Displacement limit in negative local X direction.
          * - :py:attr:`~dmxpy`
            - Get or set the Displacement limit in positive local Y direction.
          * - :py:attr:`~dmxny`
            - Get or set the Displacement limit in negative local Y direction.
          * - :py:attr:`~limpx`
            - Get or set the Action when Node 2 passes DMXPX:
          * - :py:attr:`~limnx`
            - Get or set the Action when Node 2 passes DMXNX:
          * - :py:attr:`~limpy`
            - Get or set the Action when Node 2 passes DMXPY:
          * - :py:attr:`~limny`
            - Get or set the Action when Node 2 passes DMXNY:
          * - :py:attr:`~krotx`
            - Get or set the Rotational stiffness about local X.
          * - :py:attr:`~kroty`
            - Get or set the Rotational stiffness about local Y.
          * - :py:attr:`~krotz`
            - Get or set the Rotational stiffness about local Z.
          * - :py:attr:`~tkrot`
            - Get or set the Time at which rotational stiffness becomes active.
          * - :py:attr:`~fbondh`
            - Get or set the Force to break initial bond in plane of contact surface.
          * - :py:attr:`~fbondt`
            - Get or set the Force to break initial bond in tension, normal to contact surface.
          * - :py:attr:`~dbondh`
            - Get or set the Displacement over which bond force in the plane of the contact surface reduces from FBONDH to zero.
          * - :py:attr:`~dbondt`
            - Get or set the Displacement over which bond force normal to the contact surface reduces from FBONDT to zero.
          * - :py:attr:`~lcz`
            - Get or set the Optional loadcurve ID giving force-displacement for compression in local Z (x-axis: displacement; y-axis: force).
          * - :py:attr:`~dampz`
            - Get or set the Viscous damping coefficient in local Z (additional to effect of DAMP) (force/velocity units).
          * - :py:attr:`~stiffh`
            - Get or set the Elastic stiffness in local X and Y.
          * - :py:attr:`~frmax`
            - Get or set the Upper limit on friction force.
          * - :py:attr:`~damph`
            - Get or set the Viscous damping coefficient in local X and Y (additional to effect of DAMP) (force/velocity units).
          * - :py:attr:`~gap0`
            - Get or set the Initial gap in local Z direction (length units).
          * - :py:attr:`~afac`
            - Get or set the Scale factor applied to all stiffnesses and forces.
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

    from mat_discrete_beam_point_contact import MatDiscreteBeamPointContact

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: stiff
   :type: Optional[float]


   
   Get or set the Stiffness (Force/length units).
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: Optional[float]


   
   Get or set the Friction coefficient (dimensionless).
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Damping factor (dimensionless), in the range 0 to 1. Suggested value 0.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmxpz
   :type: float


   
   Get or set the Displacement limit in positive local Z direction (uplift).
















   ..
       !! processed by numpydoc !!

.. py:property:: limpz
   :type: Optional[float]


   
   Get or set the Action when Node 2 passes DMXPZ:
   EQ.0:   element is deleted
   EQ.1:   further displacement is resisted by stiffness STIFF.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmxpx
   :type: float


   
   Get or set the Displacement limit in positive local X direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmxnx
   :type: float


   
   Get or set the Displacement limit in negative local X direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmxpy
   :type: float


   
   Get or set the Displacement limit in positive local Y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmxny
   :type: float


   
   Get or set the Displacement limit in negative local Y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: limpx
   :type: Optional[float]


   
   Get or set the Action when Node 2 passes DMXPX:
   EQ.0:   element is deleted
   EQ.1:   further displacement is resisted by stiffness STIFF.
















   ..
       !! processed by numpydoc !!

.. py:property:: limnx
   :type: Optional[float]


   
   Get or set the Action when Node 2 passes DMXNX:
   EQ.0:   element is deleted
   EQ.1:   further displacement is resisted by stiffness STIFF.
















   ..
       !! processed by numpydoc !!

.. py:property:: limpy
   :type: Optional[float]


   
   Get or set the Action when Node 2 passes DMXPY:
   EQ.0:   element is deleted
   EQ.1:   further displacement is resisted by stiffness STIFF.
















   ..
       !! processed by numpydoc !!

.. py:property:: limny
   :type: Optional[float]


   
   Get or set the Action when Node 2 passes DMXNY:
   EQ.0:   element is deleted
   EQ.1:   further displacement is resisted by stiffness STIFF.
















   ..
       !! processed by numpydoc !!

.. py:property:: krotx
   :type: Optional[float]


   
   Get or set the Rotational stiffness about local X.
















   ..
       !! processed by numpydoc !!

.. py:property:: kroty
   :type: Optional[float]


   
   Get or set the Rotational stiffness about local Y.
















   ..
       !! processed by numpydoc !!

.. py:property:: krotz
   :type: Optional[float]


   
   Get or set the Rotational stiffness about local Z.
















   ..
       !! processed by numpydoc !!

.. py:property:: tkrot
   :type: Optional[float]


   
   Get or set the Time at which rotational stiffness becomes active.
















   ..
       !! processed by numpydoc !!

.. py:property:: fbondh
   :type: Optional[float]


   
   Get or set the Force to break initial bond in plane of contact surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: fbondt
   :type: Optional[float]


   
   Get or set the Force to break initial bond in tension, normal to contact surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: dbondh
   :type: float


   
   Get or set the Displacement over which bond force in the plane of the contact surface reduces from FBONDH to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: dbondt
   :type: float


   
   Get or set the Displacement over which bond force normal to the contact surface reduces from FBONDT to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcz
   :type: int


   
   Get or set the Optional loadcurve ID giving force-displacement for compression in local Z (x-axis: displacement; y-axis: force).
















   ..
       !! processed by numpydoc !!

.. py:property:: dampz
   :type: Optional[float]


   
   Get or set the Viscous damping coefficient in local Z (additional to effect of DAMP) (force/velocity units).
















   ..
       !! processed by numpydoc !!

.. py:property:: stiffh
   :type: Optional[float]


   
   Get or set the Elastic stiffness in local X and Y.
















   ..
       !! processed by numpydoc !!

.. py:property:: frmax
   :type: Optional[float]


   
   Get or set the Upper limit on friction force.
















   ..
       !! processed by numpydoc !!

.. py:property:: damph
   :type: Optional[float]


   
   Get or set the Viscous damping coefficient in local X and Y (additional to effect of DAMP) (force/velocity units).
















   ..
       !! processed by numpydoc !!

.. py:property:: gap0
   :type: Optional[float]


   
   Get or set the Initial gap in local Z direction (length units).
















   ..
       !! processed by numpydoc !!

.. py:property:: afac
   :type: float


   
   Get or set the Scale factor applied to all stiffnesses and forces.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'DISCRETE_BEAM_POINT_CONTACT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





