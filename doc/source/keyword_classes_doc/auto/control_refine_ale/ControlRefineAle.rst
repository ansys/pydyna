





:class:`ControlRefineAle`
=========================


.. py:class:: control_refine_ale.ControlRefineAle(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_REFINE_ALE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlRefineAle

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Set ID.
          * - :py:attr:`~type`
            - Get or set the Set type:
          * - :py:attr:`~nlvl`
            - Get or set the Number of refinement levels (see Remark 3).
          * - :py:attr:`~mmsid`
            - Get or set the Multi-Material Set ID (see Remark 4):
          * - :py:attr:`~ibox`
            - Get or set the Box ID (See *DEFINE_BOX) defining a region in which the ALE elements are refined.
          * - :py:attr:`~ielout`
            - Get or set the Flag to handle child data in elout(see Remarks 10 and 11).
          * - :py:attr:`~ntotrf`
            - Get or set the Total number of ALE elements to refine (see Remark 5):
          * - :py:attr:`~ncycrf`
            - Get or set the Number of cycles between each refinement.
          * - :py:attr:`~critrf`
            - Get or set the Refinement criterion (a negative CRITRF reverses the conditions below):
          * - :py:attr:`~valrf`
            - Get or set the Criterion value to reach for the refinement.
          * - :py:attr:`~begrf`
            - Get or set the Time to begin the refinement.
          * - :py:attr:`~endrf`
            - Get or set the Time to end the refinement.
          * - :py:attr:`~layrf`
            - Get or set the Number of element layers to refine around a element reaching the refinement criterion (see Remark 6).
          * - :py:attr:`~delayrf`
            - Get or set the Period of time after removing the refinement of an element, during which this element will not be refined again.
          * - :py:attr:`~maxrm`
            - Get or set the Maximum number of child clusters to remove (see Remark 9):
          * - :py:attr:`~ncycrm`
            - Get or set the Number of cycles between each deletion.
          * - :py:attr:`~critrm`
            - Get or set the Deletion criterion(a negative CRITRM reverses the conditions below):
          * - :py:attr:`~valrm`
            - Get or set the Criterion value to reach in each child elements of a cluster for its deletion.
          * - :py:attr:`~begrm`
            - Get or set the Time to begin the deletion.
          * - :py:attr:`~endrm`
            - Get or set the Time to end the deletion.
          * - :py:attr:`~mmsrm`
            - Get or set the Multi-Material Set ID for the deletion. (See Remark 7).
          * - :py:attr:`~delayrm`
            - Get or set the Period of time after refining an element, during which this refinement will not be removed.


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

    from control_refine_ale import ControlRefineAle

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Set type:
   EQ.0: ALE Part Set,
   EQ.1: ALE Part,
   EQ.2: Lagrangian Part Set coupled to ALE (see Remarks 1 and 2),
   EQ.3: Lagrangian Part coupled to ALE (see Remarks 1 and 2),
   EQ.4: Lagrangian Shell Set coupled to ALE (see Remarks 1 and 2),
   EQ.5: ALE Solid Set.
















   ..
       !! processed by numpydoc !!

.. py:property:: nlvl
   :type: int


   
   Get or set the Number of refinement levels (see Remark 3).
















   ..
       !! processed by numpydoc !!

.. py:property:: mmsid
   :type: int


   
   Get or set the Multi-Material Set ID (see Remark 4):
   LT.0: only ALE elements with all the multi-material groups listed in
   *SET_MULTI_MATERIAL_GROUP_LIST can be refined (or re    moved otherwise)
   GT.0: ALE elements with at least one of the multi-material groups
   can be refined (or removed).
















   ..
       !! processed by numpydoc !!

.. py:property:: ibox
   :type: int


   
   Get or set the Box ID (See *DEFINE_BOX) defining a region in which the ALE elements are refined.
















   ..
       !! processed by numpydoc !!

.. py:property:: ielout
   :type: int


   
   Get or set the Flag to handle child data in elout(see Remarks 10 and 11).
















   ..
       !! processed by numpydoc !!

.. py:property:: ntotrf
   :type: int


   
   Get or set the Total number of ALE elements to refine (see Remark 5):
   GT.0:   Number of elements to refine
   EQ.0:   Number of shell elements in IBOX (see Remark 2)
   EQ.-1: Add clusters of 4 shells for the refinement during the run.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycrf
   :type: float


   
   Get or set the Number of cycles between each refinement.
   LT.0: |NCYCRF| is the time interval.
















   ..
       !! processed by numpydoc !!

.. py:property:: critrf
   :type: int


   
   Get or set the Refinement criterion (a negative CRITRF reverses the conditions below):
   EQ.0: static refinement (as if only the 1st card is defined),
   EQ.1: Pressure (if pressure > VALRF),
   EQ.2: Relative Volume (if V/Vo < VALRF) ,
   EQ.3: Volume Fraction (if Volume fraction > VALRF),
   EQ.5: User defined criterion. The fortran routine alerfn_criteria5 in the
   file dynrfn_user.f should be used to develop the criterion. The file is
   part of the general package usermat.
















   ..
       !! processed by numpydoc !!

.. py:property:: valrf
   :type: float


   
   Get or set the Criterion value to reach for the refinement.
















   ..
       !! processed by numpydoc !!

.. py:property:: begrf
   :type: float


   
   Get or set the Time to begin the refinement.
















   ..
       !! processed by numpydoc !!

.. py:property:: endrf
   :type: float


   
   Get or set the Time to end the refinement.
















   ..
       !! processed by numpydoc !!

.. py:property:: layrf
   :type: int


   
   Get or set the Number of element layers to refine around a element reaching the refinement criterion (see Remark 6).
















   ..
       !! processed by numpydoc !!

.. py:property:: delayrf
   :type: float


   
   Get or set the Period of time after removing the refinement of an element, during which this element will not be refined again.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxrm
   :type: int


   
   Get or set the Maximum number of child clusters to remove (see Remark 9):
   LT.0: for the whole run,
   GT.0: every NCYCRM cycles.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycrm
   :type: float


   
   Get or set the Number of cycles between each deletion.
   LT.0: |NCYCRM| is the time interval.
















   ..
       !! processed by numpydoc !!

.. py:property:: critrm
   :type: int


   
   Get or set the Deletion criterion(a negative CRITRM reverses the conditions below):
   EQ.0: no deletion (as if only the 1st and 2nd card are defined),
   EQ.1: Pressure (if pressure < VALRM),
   EQ.2: Relative Volume (if V/Vo > VALRM) ,
   EQ.3: Volume Fraction (if Volume fraction < VALRM),
   EQ.5: User defined criterion. The fortran routine alermv_criteria5 in
   the file dynrfn_user.f should be used to develop the criterion. The file is
   part of the general package usermat.
















   ..
       !! processed by numpydoc !!

.. py:property:: valrm
   :type: float


   
   Get or set the Criterion value to reach in each child elements of a cluster for its deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: begrm
   :type: float


   
   Get or set the Time to begin the deletion.
   LT.0: |BEGRM| represents a critical percent of NTOTRF below
   which the deletion should begin (0.0 < |BEGRM| < 1.0). (See Remark 8).
















   ..
       !! processed by numpydoc !!

.. py:property:: endrm
   :type: float


   
   Get or set the Time to end the deletion.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmsrm
   :type: int


   
   Get or set the Multi-Material Set ID for the deletion. (See Remark 7).
















   ..
       !! processed by numpydoc !!

.. py:property:: delayrm
   :type: float


   
   Get or set the Period of time after refining an element, during which this refinement will not be removed.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'REFINE_ALE'






