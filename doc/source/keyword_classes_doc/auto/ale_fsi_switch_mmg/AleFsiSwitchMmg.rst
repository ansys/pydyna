





:class:`AleFsiSwitchMmg`
========================


.. py:class:: ale_fsi_switch_mmg.AleFsiSwitchMmg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_FSI_SWITCH_MMG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleFsiSwitchMmg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Switch list ID,
          * - :py:attr:`~title`
            - Get or set the Switch list title .
          * - :py:attr:`~sid`
            - Get or set the A set ID defining a monitoring surface over which an ALE fluid flows across, and its ALE multi-material-group-ID (AMMGID) is switched.  The monitoring surface may be a Lagrangian shell structure, or simply a segment set, and it does not have to be included in the coupling definition.
          * - :py:attr:`~stype`
            - Get or set the Set ID type of the above SID.
          * - :py:attr:`~nquad`
            - Get or set the The number of flow-sensor points to be distributed over each monitoring surface/segment.  There should be enough sensor points distributed to monitor the flow in each ALE element intersected by this monitoring surface (default=1).
          * - :py:attr:`~xoff`
            - Get or set the Offset distance away from the monitoring surface, beyond which the AMMGID is switched.  The direction of XOFF depends on the normal vector of the monitoring segment.  This offset distance should be at least 1 ALE element width away from, and beyond the monitoring interface (default=0.0).
          * - :py:attr:`~btime`
            - Get or set the Start time for the AMMGID switch to be activated (default=0.0).
          * - :py:attr:`~dtime`
            - Get or set the Ending time for the AMMGID switch (default=1.0E20).
          * - :py:attr:`~nfreq`
            - Get or set the Number of computational cycles between ALE switch check (default=1).
          * - :py:attr:`~nfold`
            - Get or set the Flag for checking folding logic (default=0=off).  If NFOLD=1=on, then LS-DYNA will check if the monitoring segment is in the fold, applicable to airbag.  If the monitoring segment is still located within a folded (shell) region, then no switching is allowed yet until it has unfolded.
          * - :py:attr:`~fr_mmg`
            - Get or set the This is the AMMG-SID before the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.  This SID points to one or more AMMGs (remark 1).
          * - :py:attr:`~to_mmg`
            - Get or set the This is the AMMG-SID after the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST card. This SID points to one or more AMMGs (remark 1).
          * - :py:attr:`~xclen`
            - Get or set the This is an absolute distance for distributing the flow sensor points over over the ALE elements.  To make sure that at least 1 sensor point, defined on each Lagrangian segment, is present in each ALE element to track the flow of an AMMG, XLEN may be estimated as roughly half the length of the smallest ALE element in the mesh.  This overwrites the NQUAD distribution of sensor points (default=0.0).


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

    from ale_fsi_switch_mmg import AleFsiSwitchMmg

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Switch list ID,
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Switch list title .
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the A set ID defining a monitoring surface over which an ALE fluid flows across, and its ALE multi-material-group-ID (AMMGID) is switched.  The monitoring surface may be a Lagrangian shell structure, or simply a segment set, and it does not have to be included in the coupling definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Set ID type of the above SID.
   EQ.0: Part set ID (PSID) (default).
   EQ.1: Part ID (PID).
   EQ.2: Segment set ID (SGSID)
















   ..
       !! processed by numpydoc !!

.. py:property:: nquad
   :type: int


   
   Get or set the The number of flow-sensor points to be distributed over each monitoring surface/segment.  There should be enough sensor points distributed to monitor the flow in each ALE element intersected by this monitoring surface (default=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: xoff
   :type: float


   
   Get or set the Offset distance away from the monitoring surface, beyond which the AMMGID is switched.  The direction of XOFF depends on the normal vector of the monitoring segment.  This offset distance should be at least 1 ALE element width away from, and beyond the monitoring interface (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: btime
   :type: float


   
   Get or set the Start time for the AMMGID switch to be activated (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: dtime
   :type: float


   
   Get or set the Ending time for the AMMGID switch (default=1.0E20).
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of computational cycles between ALE switch check (default=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: nfold
   :type: int


   
   Get or set the Flag for checking folding logic (default=0=off).  If NFOLD=1=on, then LS-DYNA will check if the monitoring segment is in the fold, applicable to airbag.  If the monitoring segment is still located within a folded (shell) region, then no switching is allowed yet until it has unfolded.
















   ..
       !! processed by numpydoc !!

.. py:property:: fr_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMG-SID before the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.  This SID points to one or more AMMGs (remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: to_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMG-SID after the switch.  The AMMG-SID corresponds to the SID defined under the *SET_MULTI-MATERIAL_GROUP_LIST card. This SID points to one or more AMMGs (remark 1).
















   ..
       !! processed by numpydoc !!

.. py:property:: xclen
   :type: float


   
   Get or set the This is an absolute distance for distributing the flow sensor points over over the ALE elements.  To make sure that at least 1 sensor point, defined on each Lagrangian segment, is present in each ALE element to track the flow of an AMMG, XLEN may be estimated as roughly half the length of the smallest ALE element in the mesh.  This overwrites the NQUAD distribution of sensor points (default=0.0).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'FSI_SWITCH_MMG'






