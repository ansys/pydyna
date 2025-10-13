





:class:`DefineCpmVent`
======================


.. py:class:: define_cpm_vent.DefineCpmVent(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CPM_VENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCpmVent

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Unique ID for this card
          * - :py:attr:`~c23`
            - Get or set the Vent hole coefficient (Parameter for Wang-Nefske leakage) (Default 1.0) See Remark 1 below.
          * - :py:attr:`~lctc23`
            - Get or set the Load curve defining vent hole coefficient as a function of time.
          * - :py:attr:`~lcpc23`
            - Get or set the Load curve defining vent hole coefficient as a function of pressure.
          * - :py:attr:`~enh_v`
            - Get or set the Enhance venting option. (Default 0)  However if Joule-Thomson effect is considered, the option will set to 1 automatically.
          * - :py:attr:`~ppop`
            - Get or set the Pressure difference between interior and ambient pressure to open the vent hole.Once the vent is open it will stay open.
          * - :py:attr:`~c23up`
            - Get or set the Scale factor of C23 while switching from CPM to uniform pressure calculation
          * - :py:attr:`~iopt`
            - Get or set the Directional venting:
          * - :py:attr:`~jt`
            - Get or set the Joule-Thomson effect, If Joule-Thomson effect is considered, ENH_V will set to enable.
          * - :py:attr:`~ids1`
            - Get or set the JT's up stream condition part ID/chamber ID
          * - :py:attr:`~ids2`
            - Get or set the JT's downstream condition part ID/chamber ID
          * - :py:attr:`~iopt1`
            - Get or set the Upstream chamber ID for one-way vent hole.  This will help the code to determine the probability function.
          * - :py:attr:`~pid1`
            - Get or set the PID1 and PID2 indicate parts for determining the local part pressures that can be used to evaluate the vent probability function. Depending on if a chamber is defined, how the local part pressures are evaluated changes (see *DEFINE_CPM_CHAMBER). PID1 and PID2 are optional if a chamber is defined, otherwise they are required input.
          * - :py:attr:`~pid2`
            - Get or set the PID1 and PID2 indicate parts for determining the local part pressures that can be used to evaluate the vent probability function. Depending on if a chamber is defined, how the local part pressures are evaluated changes (see *DEFINE_CPM_CHAMBER). PID1 and PID2 are optional if a chamber is defined, otherwise they are required input.
          * - :py:attr:`~vang`
            - Get or set the Cone angle in degrees. Particle goes through this vent will be redirection based on this angle.  This option is only valid with internal vent.
          * - :py:attr:`~lcred`
            - Get or set the Time dependent probability curve to control CPM particle through the internal vent with VANG option.
          * - :py:attr:`~nid1`
            - Get or set the Three nodes define a moving coordinate system for the direction of flow through the vent when VANG equals -2
          * - :py:attr:`~nid2`
            - Get or set the Three nodes define a moving coordinate system for the direction of flow through the vent when VANG equals -2
          * - :py:attr:`~lcac23`
            - Get or set the Load curve defining vent hole coefficient as a function of current vent area.
          * - :py:attr:`~psetpv`
            - Get or set the |PSETPV | is a part set ID for internal airbag parts that interact with the push-out vent (IOPT = 200). The sign determines where the ambient pressure is applied:
          * - :py:attr:`~sfpv`
            - Get or set the Scale factor for the characteristic length of the element.  CL is defined as sqrt(element area).
          * - :py:attr:`~lpatm`
            - Get or set the Load curve for ambient pressure of the external vent.  This option only works for the CPM mode.
          * - :py:attr:`~jtnd`
            - Get or set the Node/Node Set for applying vent reaction force
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

    from define_cpm_vent import DefineCpmVent

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Unique ID for this card
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: float


   
   Get or set the Vent hole coefficient (Parameter for Wang-Nefske leakage) (Default 1.0) See Remark 1 below.
















   ..
       !! processed by numpydoc !!

.. py:property:: lctc23
   :type: Optional[int]


   
   Get or set the Load curve defining vent hole coefficient as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcpc23
   :type: Optional[int]


   
   Get or set the Load curve defining vent hole coefficient as a function of pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: enh_v
   :type: int


   
   Get or set the Enhance venting option. (Default 0)  However if Joule-Thomson effect is considered, the option will set to 1 automatically.
   EQ.0: Disable
   EQ.1: Enable
















   ..
       !! processed by numpydoc !!

.. py:property:: ppop
   :type: Optional[float]


   
   Get or set the Pressure difference between interior and ambient pressure to open the vent hole.Once the vent is open it will stay open.
















   ..
       !! processed by numpydoc !!

.. py:property:: c23up
   :type: Optional[float]


   
   Get or set the Scale factor of C23 while switching from CPM to uniform pressure calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: iopt
   :type: Optional[int]


   
   Get or set the Directional venting:
   EQ.1:   In shell normal
   EQ.2:   Against shell normal
   One-way venting:
   EQ.10:  In shell normal
   EQ.20:  Against shell normal
   Special vent option::
   EQ.100: Enable compression seal vent. Vent area is adjusted according to the formula below. See Remark 1.A_
   EQ.200: Enable push-out vent. Particle remains active while going through this external vent within the range of 2 times of its characteristic length
















   ..
       !! processed by numpydoc !!

.. py:property:: jt
   :type: int


   
   Get or set the Joule-Thomson effect, If Joule-Thomson effect is considered, ENH_V will set to enable.
   EQ.0: Disable
   EQ.1: Use part pressure
   EQ.2: Use chamber pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: ids1
   :type: Optional[int]


   
   Get or set the JT's up stream condition part ID/chamber ID
















   ..
       !! processed by numpydoc !!

.. py:property:: ids2
   :type: Optional[int]


   
   Get or set the JT's downstream condition part ID/chamber ID
   EQ.0: airbag's PATM will be used for downstream pressure
















   ..
       !! processed by numpydoc !!

.. py:property:: iopt1
   :type: Optional[int]


   
   Get or set the Upstream chamber ID for one-way vent hole.  This will help the code to determine the probability function.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid1
   :type: Optional[int]


   
   Get or set the PID1 and PID2 indicate parts for determining the local part pressures that can be used to evaluate the vent probability function. Depending on if a chamber is defined, how the local part pressures are evaluated changes (see *DEFINE_CPM_CHAMBER). PID1 and PID2 are optional if a chamber is defined, otherwise they are required input.
   When a chamber is defined, specifying PID1and PID2 causes the vent probability function to be evaluated from the difference of local part pressures between PID1and PID2.Otherwise the calculation involves the chamber pressure.This option is usually used for vents near a long sleeve which causes unrealistic venting using chamber pressure alone.
   When a chamber is not defined, the vent probability function is evaluated from the difference of local part pressures between PID1and PID2, using the location of the part centers to help determine vent direction..If the part is an external part, the part pressure will be used.If the part is an internal part, the pressure on the shell’s positive normal side will be used.If the vent is an external vent, PID1 should be the same as PID2 to avoid input error.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid2
   :type: Optional[int]


   
   Get or set the PID1 and PID2 indicate parts for determining the local part pressures that can be used to evaluate the vent probability function. Depending on if a chamber is defined, how the local part pressures are evaluated changes (see *DEFINE_CPM_CHAMBER). PID1 and PID2 are optional if a chamber is defined, otherwise they are required input.
   When a chamber is defined, specifying PID1and PID2 causes the vent probability function to be evaluated from the difference of local part pressures between PID1and PID2.Otherwise the calculation involves the chamber pressure.This option is usually used for vents near a long sleeve which causes unrealistic venting using chamber pressure alone.
   When a chamber is not defined, the vent probability function is evaluated from the difference of local part pressures between PID1and PID2, using the location of the part centers to help determine vent direction..If the part is an external part, the part pressure will be used.If the part is an internal part, the pressure on the shell’s positive normal side will be used.If the vent is an external vent, PID1 should be the same as PID2 to avoid input error.
















   ..
       !! processed by numpydoc !!

.. py:property:: vang
   :type: float


   
   Get or set the Cone angle in degrees. Particle goes through this vent will be redirection based on this angle.  This option is only valid with internal vent.
   GT.0:   cone angle (maximum 270)
   EQ.0: disabled (Default)
   EQ.-1: direction follows the vent normal
   EQ.-2: direction follows local coordinates system defined by the following three nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: lcred
   :type: Optional[int]


   
   Get or set the Time dependent probability curve to control CPM particle through the internal vent with VANG option.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid1
   :type: Optional[int]


   
   Get or set the Three nodes define a moving coordinate system for the direction of flow through the vent when VANG equals -2
















   ..
       !! processed by numpydoc !!

.. py:property:: nid2
   :type: Optional[int]


   
   Get or set the Three nodes define a moving coordinate system for the direction of flow through the vent when VANG equals -2
















   ..
       !! processed by numpydoc !!

.. py:property:: lcac23
   :type: Optional[int]


   
   Get or set the Load curve defining vent hole coefficient as a function of current vent area.
















   ..
       !! processed by numpydoc !!

.. py:property:: psetpv
   :type: Optional[int]


   
   Get or set the |PSETPV | is a part set ID for internal airbag parts that interact with the push-out vent (IOPT = 200). The sign determines where the ambient pressure is applied:
   GT.0:   Ambient pressure is applied to elements in these parts that are at least a distance of SFPV×CL  away from the vent.
   LT.0 : Ambient pressure is applied to elements in these parts that are within a distance of SFPV×CL  of the vent.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfpv
   :type: Optional[int]


   
   Get or set the Scale factor for the characteristic length of the element.  CL is defined as sqrt(element area).
















   ..
       !! processed by numpydoc !!

.. py:property:: lpatm
   :type: Optional[int]


   
   Get or set the Load curve for ambient pressure of the external vent.  This option only works for the CPM mode.
















   ..
       !! processed by numpydoc !!

.. py:property:: jtnd
   :type: Optional[int]


   
   Get or set the Node/Node Set for applying vent reaction force
   GT.0:   Node ID
   LT.0 : Node Set ID.The average force is evenly applied among the nodes in the node set.
















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
   :value: 'CPM_VENT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





