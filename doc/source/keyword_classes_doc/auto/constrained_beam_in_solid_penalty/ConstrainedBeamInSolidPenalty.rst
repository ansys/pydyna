





:class:`ConstrainedBeamInSolidPenalty`
======================================


.. py:class:: constrained_beam_in_solid_penalty.ConstrainedBeamInSolidPenalty(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_BEAM_IN_SOLID_PENALTY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedBeamInSolidPenalty

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
          * - :py:attr:`~bside`
            - Get or set the Part or part set ID of the Lagrangian beam structure(see *PART,* SET_PART)
          * - :py:attr:`~ssid`
            - Get or set the Part or part set ID of the Lagrangian solid elements or thick shell element(see *PART,* SET_PART)
          * - :py:attr:`~bstyp`
            - Get or set the Set type of BSID
          * - :py:attr:`~sstyp`
            - Get or set the Set type of SSID
          * - :py:attr:`~ncoup_`
            - Get or set the Number of coupling points generated in one beam element. If set to 0, coupling only happens at beam nodes. Otherwise, coupling is done at both the beam nodes and those automatically generated coupling points
          * - :py:attr:`~cdir`
            - Get or set the Coupling direction.
          * - :py:attr:`~start`
            - Get or set the Start time to activate the coupling
          * - :py:attr:`~end`
            - Get or set the End time to deactive the coupling
          * - :py:attr:`~axfor_`
            - Get or set the ID of a user defined function describes coupling force versus slip along beam axial direction.
          * - :py:attr:`~pssf`
            - Get or set the Penalty spring stiffness scale factor. Only available in penalty form.
          * - :py:attr:`~xint`
            - Get or set the Interval distance. This field is designed to deal with beam elements having a wide variation in lengths.


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

    from constrained_beam_in_solid_penalty import ConstrainedBeamInSolidPenalty

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

.. py:property:: bside
   :type: Optional[int]


   
   Get or set the Part or part set ID of the Lagrangian beam structure(see *PART,* SET_PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Part or part set ID of the Lagrangian solid elements or thick shell element(see *PART,* SET_PART)
















   ..
       !! processed by numpydoc !!

.. py:property:: bstyp
   :type: int


   
   Get or set the Set type of BSID
   EQ.0: part set ID (PSID).
   EQ.1: part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: sstyp
   :type: int


   
   Get or set the Set type of SSID
   EQ.0: part set ID (PSID).
   EQ.1: part ID (PID).
















   ..
       !! processed by numpydoc !!

.. py:property:: ncoup_
   :type: Optional[int]


   
   Get or set the Number of coupling points generated in one beam element. If set to 0, coupling only happens at beam nodes. Otherwise, coupling is done at both the beam nodes and those automatically generated coupling points
















   ..
       !! processed by numpydoc !!

.. py:property:: cdir
   :type: Optional[int]


   
   Get or set the Coupling direction.
   EQ.0: default, constraint applied along all directions.
   EQ.1: Constraint only applied along normal directions; along the beam axial direction there is no constraint
















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

.. py:property:: axfor_
   :type: Optional[int]


   
   Get or set the ID of a user defined function describes coupling force versus slip along beam axial direction.
   GE.0: OFF
   EQ.-n: n is the function ID in *DEFINE_FUNCTION
















   ..
       !! processed by numpydoc !!

.. py:property:: pssf
   :type: float


   
   Get or set the Penalty spring stiffness scale factor. Only available in penalty form.
















   ..
       !! processed by numpydoc !!

.. py:property:: xint
   :type: Optional[int]


   
   Get or set the Interval distance. This field is designed to deal with beam elements having a wide variation in lengths.
   Coupling points are generated at an interval of length equal to XINT.
   Hence the number of coupling points in a beam element is no longer a fixed number (NCOUP),
   but rather variable, depending on the length of the beam element.
   This field can be used together with NCOUP.
   In that case, in each element, we will take the larger number of coupling points from these two options.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'BEAM_IN_SOLID_PENALTY'






