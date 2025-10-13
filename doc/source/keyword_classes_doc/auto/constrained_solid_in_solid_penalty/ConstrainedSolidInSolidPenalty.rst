





:class:`ConstrainedSolidInSolidPenalty`
=======================================


.. py:class:: constrained_solid_in_solid_penalty.ConstrainedSolidInSolidPenalty(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_SOLID_IN_SOLID_PENALTY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedSolidInSolidPenalty

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~coupid`
            - Get or set the Coupling card ID number
          * - :py:attr:`~title`
            - Get or set the A description of this coupling definition
          * - :py:attr:`~ssidb`
            - Get or set the Set ID defining a part or part set ID of the Lagrangian solid structure constrained to move with solid or thick shell elementsspecified with SSIDA(see * PART and *SET_PART).
          * - :py:attr:`~ssida`
            - Get or set the Set ID defining a part or part set ID of the Lagrangian solid elements or thick shell elements which constrain SSIDB(see * PART and *SET_PART).
          * - :py:attr:`~bstypb`
            - Get or set the Set type of SSIDB
          * - :py:attr:`~sstypa`
            - Get or set the Set type of SSIDA
          * - :py:attr:`~start`
            - Get or set the Start time to activate the coupling
          * - :py:attr:`~end`
            - Get or set the End time to deactive the coupling
          * - :py:attr:`~pssf`
            - Get or set the Penalty spring stiffness scale factor. Only available in penalty form.


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

    from constrained_solid_in_solid_penalty import ConstrainedSolidInSolidPenalty

Property detail
---------------

.. py:property:: coupid
   :type: Optional[int]


   
   Get or set the Coupling card ID number
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the A description of this coupling definition
















   ..
       !! processed by numpydoc !!

.. py:property:: ssidb
   :type: Optional[int]


   
   Get or set the Set ID defining a part or part set ID of the Lagrangian solid structure constrained to move with solid or thick shell elementsspecified with SSIDA(see * PART and *SET_PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: ssida
   :type: Optional[int]


   
   Get or set the Set ID defining a part or part set ID of the Lagrangian solid elements or thick shell elements which constrain SSIDB(see * PART and *SET_PART).
















   ..
       !! processed by numpydoc !!

.. py:property:: bstypb
   :type: int


   
   Get or set the Set type of SSIDB
   EQ.0: part set ID (PSID).
   EQ.1: part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: sstypa
   :type: int


   
   Get or set the Set type of SSIDA
   EQ.0: part set ID (PSID).
   EQ.1: part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Start time to activate the coupling
   LT.0:   Start time is set to |START|.  When negative, start time is followed during the dynamic relaxation phase of the calculation.  After dynamic relaxation has completed, coupling is activated regardless of the value of END.EQ.0:        Start time is inactive, meaning coupling is always active
   GT.0 : If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start - time and end - time.Otherwise, if END > 0, start time applies both duringand after dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: end
   :type: float


   
   Get or set the End time to deactive the coupling
   LT.0:   If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start-time and end-time.  Otherwise, negative END indicates that coupling is inactive during dynamic relaxation.  After dynamic relaxation the start and end times are followed and set to |START| and |END|, respectively.EQ.0:       END defaults to 1020.
   GT.0 : END sets the time at which the coupling is deactivated.
















   ..
       !! processed by numpydoc !!

.. py:property:: pssf
   :type: float


   
   Get or set the Penalty spring stiffness scale factor. Only available in penalty form.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'SOLID_IN_SOLID_PENALTY'






