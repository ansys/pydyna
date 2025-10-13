





:class:`AleUpSwitch`
====================


.. py:class:: ale_up_switch.AleUpSwitch(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_UP_SWITCH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleUpSwitch

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~upid`
            - Get or set the An ID defines a corresponding *AIRBAG_HYBRID_ID card for use in
          * - :py:attr:`~swtime`
            - Get or set the The time at which the computation does a switch from an ALE-method-to-CV-method.
          * - :py:attr:`~fsi_id1`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~fsi_id2`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~fsi_id3`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~fsi_id4`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~fsi_id5`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~fsi_id6`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~fsi_id7`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~fsi_id8`
            - Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
          * - :py:attr:`~sid`
            - Get or set the A set ID defines the Lagrangian parts which make up the airbag.
          * - :py:attr:`~sidtype`
            - Get or set the Set ID type for the above SETID (following the conventions in
          * - :py:attr:`~mmgair`
            - Get or set the The AMMG (ALE multi-material group) ID of surrounding air.
          * - :py:attr:`~mmggas`
            - Get or set the The AMMG ID of inflator gas injected into the airbag.


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

    from ale_up_switch import AleUpSwitch

Property detail
---------------

.. py:property:: upid
   :type: int


   
   Get or set the An ID defines a corresponding *AIRBAG_HYBRID_ID card for use in
   an ALE-method-switching-to-CV-method simulation. The simulation
   starts with ALE computational method, then switches to a CV (or UP)
   method at some given time.
   EQ.0: (or blank) The code will construct an equivalent
   *AIRBAG_HYBRID_ID card automatically internally, (default).
   The 3rd optional line is then a required input.
   NE.0: An ID points to a corresponding *AIRBAG_HYBRID_ID
   card which must be defined for use after the switch. If UPID is
   defined, do not define the 3rd optional card.
















   ..
       !! processed by numpydoc !!

.. py:property:: swtime
   :type: float


   
   Get or set the The time at which the computation does a switch from an ALE-method-to-CV-method.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id1
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id2
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id3
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id4
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id5
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id6
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id7
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsi_id8
   :type: int


   
   Get or set the Coupling IDs for one or more ALE fluid-structure-interaction (FSI)
   *CONSTRAINED_LAGRANGE_IN_SOLID_ID cards. These couplings are deleted during the 2nd, CV computational phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: int


   
   Get or set the A set ID defines the Lagrangian parts which make up the airbag.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtype
   :type: int


   
   Get or set the Set ID type for the above SETID (following the conventions in
   *AIRBAG_HYBRID card).
   EQ.0: SID is a segment set ID (SGSID).
   NE.0: SID is a part set ID (PSID).
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgair
   :type: int


   
   Get or set the The AMMG (ALE multi-material group) ID of surrounding air.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmggas
   :type: int


   
   Get or set the The AMMG ID of inflator gas injected into the airbag.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'UP_SWITCH'






