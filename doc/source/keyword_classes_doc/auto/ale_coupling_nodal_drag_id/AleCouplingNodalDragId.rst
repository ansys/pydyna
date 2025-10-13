





:class:`AleCouplingNodalDragId`
===============================


.. py:class:: ale_coupling_nodal_drag_id.AleCouplingNodalDragId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_COUPLING_NODAL_DRAG_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleCouplingNodalDragId

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~coupid`
            - Get or set the Coupling (card) ID number (I10). If not defined, LSDYNA will assign an internal coupling ID based on the order of appearance in the input deck.
          * - :py:attr:`~title`
            - Get or set the A description of this coupling definition (A70).
          * - :py:attr:`~strsid`
            - Get or set the Set ID defining a part, part set or segment set ID of the particles (see *PART, *SET_PART or *SET_SEGMENT).The particles can be SPH or discrete elements
          * - :py:attr:`~alesid`
            - Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_PART, and see Remark 1)
          * - :py:attr:`~strsty`
            - Get or set the Particle set type:
          * - :py:attr:`~alesty`
            - Get or set the Master set type of "MASTER"
          * - :py:attr:`~start`
            - Get or set the Start time for coupling.
          * - :py:attr:`~end`
            - Get or set the End time for coupling.
          * - :py:attr:`~fcoef`
            - Get or set the Drag coefficient scale factor or function ID to calculate drag coefficient
          * - :py:attr:`~direcg`
            - Get or set the Gravity force direction.
          * - :py:attr:`~grav`
            - Get or set the Gravity value. This value is used to calculate buoyance force


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

    from ale_coupling_nodal_drag_id import AleCouplingNodalDragId

Property detail
---------------

.. py:property:: coupid
   :type: Optional[int]


   
   Get or set the Coupling (card) ID number (I10). If not defined, LSDYNA will assign an internal coupling ID based on the order of appearance in the input deck.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the A description of this coupling definition (A70).
















   ..
       !! processed by numpydoc !!

.. py:property:: strsid
   :type: Optional[int]


   
   Get or set the Set ID defining a part, part set or segment set ID of the particles (see *PART, *SET_PART or *SET_SEGMENT).The particles can be SPH or discrete elements
















   ..
       !! processed by numpydoc !!

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the Set ID defining a part or part set ID of the ALE solid elements (see *PART or *SET_PART, and see Remark 1)
















   ..
       !! processed by numpydoc !!

.. py:property:: strsty
   :type: int


   
   Get or set the Particle set type:
   EQ.0: Part set ID (PSID).
   EQ.1: Part ID (PID).
   EQ.2: Segment set ID (SSID).
   EQ.3: Node set ID (NSID).
















   ..
       !! processed by numpydoc !!

.. py:property:: alesty
   :type: int


   
   Get or set the Master set type of "MASTER"
   EQ.0: Part set ID (PSID).
   EQ.1: Part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Start time for coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: end
   :type: float


   
   Get or set the End time for coupling.
















   ..
       !! processed by numpydoc !!

.. py:property:: fcoef
   :type: int


   
   Get or set the Drag coefficient scale factor or function ID to calculate drag coefficient
   GT.0:   Drag coefficient scale factor.
   LT.0 : The absolute value of FCOEF is the Function ID of the user provided function to calculate drag coefficient; See Remark 1
















   ..
       !! processed by numpydoc !!

.. py:property:: direcg
   :type: int


   
   Get or set the Gravity force direction.
   EQ.1:   Global x direction
   EQ.2 : Global y direction
   EQ.3 : Global z direction
















   ..
       !! processed by numpydoc !!

.. py:property:: grav
   :type: float


   
   Get or set the Gravity value. This value is used to calculate buoyance force
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'COUPLING_NODAL_DRAG_ID'






