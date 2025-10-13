





:class:`ControlContact`
=======================


.. py:class:: control_contact.ControlContact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_CONTACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlContact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~slsfac`
            - Get or set the Scale factor for sliding interface penalties (default = 0.1)
          * - :py:attr:`~rwpnal`
            - Get or set the Scale factor for rigid wall penalties, which treat nodal points interacting with rigid walls, RWPNAL.  The penalties are set so that an absolute value of unity should be optimal; however, this penalty value may be very problem dependent.  If rigid/deformable materials switching is used, this option should be used if the switched materials are interacting with rigid walls.
          * - :py:attr:`~islchk`
            - Get or set the Initial penetration check in contact surfaces.
          * - :py:attr:`~shlthk`
            - Get or set the Shell thickness considered in type surface to surface and node to surface type contact options, where options 1 and 2 below activate the new contact algorithms.
          * - :py:attr:`~penopt`
            - Get or set the Penalty stiffness value option.
          * - :py:attr:`~thkchg`
            - Get or set the Shell thickness changes considered in single surface contact:
          * - :py:attr:`~orien`
            - Get or set the Optional automatic reorientation of contact interface segments during initialization:
          * - :py:attr:`~enmass`
            - Get or set the Treatment of the mass of eroded nodes in contact. This option effects all contact types where nodes are removed after surrounding elements fail. Generally, the removal of eroded nodes makes the calculation more stable; however, in problems where erosion is important the reduction of mass will lead to incorrect results.
          * - :py:attr:`~usrstr`
            - Get or set the Storage per contact interface for user supplied interface control subroutine.  If zero, no input data is read.
          * - :py:attr:`~usrfrc`
            - Get or set the Storage per contact interface for user supplied interface friction subroutine. If zero, no input data is read.
          * - :py:attr:`~nsbcs`
            - Get or set the Number of cycles between contact searching. Values between 10-100 recommended.
          * - :py:attr:`~interm`
            - Get or set the Flag for intermittent searching in old surface to surface contact using the interval specified as NSBCS above:
          * - :py:attr:`~xpene`
            - Get or set the Contact surface maximum penetration check multiplier
          * - :py:attr:`~ssthk`
            - Get or set the Flag for using actual shell thickness in single surface contact logic-types 4,13,15 and 26.
          * - :py:attr:`~ecdt`
            - Get or set the Time step size override for eroding contact:
          * - :py:attr:`~tiedprj`
            - Get or set the Bypass projection of slave nodes to master surface in types:
          * - :py:attr:`~sfric`
            - Get or set the Default static coefficient of friction (see *PART_CONTACT).
          * - :py:attr:`~dfric`
            - Get or set the Default dynamic coefficient of friction (see *PART_CONTACT).
          * - :py:attr:`~edc`
            - Get or set the Default exponential decay coefficient (see *PART_CONTACT).
          * - :py:attr:`~vfc`
            - Get or set the Default viscous friction coefficient (see *PART_CONTACT).
          * - :py:attr:`~th`
            - Get or set the Default contact thickness (see *PART_CONTACT).
          * - :py:attr:`~th_sf`
            - Get or set the Default thickness scale factor (see *PART_CONTACT).
          * - :py:attr:`~pen_sf`
            - Get or set the Default local penalty scale factor (see *PART_CONTACT).
          * - :py:attr:`~ignore`
            - Get or set the Ignore initial penetrations in the *CONTACT_AUTOMATIC options. This option can also be specified for each interface on the third optional card under the keyword, *CONTACT. The value defined here will be the default.
          * - :py:attr:`~frceng`
            - Get or set the Flag to activate the calculation of frictional sliding energy:
          * - :py:attr:`~skiprwg`
            - Get or set the Flag not to display stationary rigid wall by default.
          * - :py:attr:`~outseg`
            - Get or set the Flag to output each spot weld slave node and its master segment for contact type: *CONTACT_SPOTWELD into the D3HSP file.
          * - :py:attr:`~spotstp`
            - Get or set the If a spot weld node (related to a *MAT_SPOTWELD beam) cannot be fouind on a master segment, should an error termination occur?
          * - :py:attr:`~spotdel`
            - Get or set the If a spot weld node of a spot weld beam is attached to a shell element, which fails and is deleted, then the attached spot weld beam element is deleted if this flag is on. There is a small cost penalty realted to this option on non-vector processors. On vector processors, however, this option can significantly slow down the calculation if many weld elements fail since the vector lengths are reduced.
          * - :py:attr:`~spothin`
            - Get or set the Optional thickness scale factor. If active, define a factor greater than zero, but less than one.
          * - :py:attr:`~isym`
            - Get or set the Symmetry plane default for automatic segment generation when contact is defined by part IDs:
          * - :py:attr:`~nserod`
            - Get or set the Flag to use one way node to surface erosion:
          * - :py:attr:`~rwgaps`
            - Get or set the Flag to add rigid wall gap stiffness, see parameter RWGDTH below.
          * - :py:attr:`~rwgdth`
            - Get or set the Death time for gap stiffness. After this time the gap stiffness is no longer added.
          * - :py:attr:`~rwksf`
            - Get or set the Rigid wall penalty scale factor for contact with deformable parts during implicit calculations.  This value is independent of SLSFAC and RWPNAL. If RWKSF is also specified in *RIGIDWALL_PLANAR, the stiffness is scaled by the product of the two values..
          * - :py:attr:`~icov`
            - Get or set the Invokes the covariant formulation of Konyukhov and Schweizerhof in the FORMING contact option.
          * - :py:attr:`~swradf`
            - Get or set the Spot weld radius scale factor for neighbor segment thinning:EQ.0:        Neighbor segments are not thinned(default).GT.0 : The radius of a spot weld is scaled by SWRADF when searching for close neighbor segments to thin.
          * - :py:attr:`~ithoff`
            - Get or set the Thermal contact heat transfer position.
          * - :py:attr:`~shledg`
            - Get or set the Flag for assuming edge shape for shells when measuring penetration.
          * - :py:attr:`~pstiff`
            - Get or set the Flag to choose the method for calculating the penalty stiffness. This is
          * - :py:attr:`~ithcnt`
            - Get or set the Thermal contact heat transfer methodology
          * - :py:attr:`~tdcnof`
            - Get or set the Tied constraint offset contact update option.
          * - :py:attr:`~ftall`
            - Get or set the Option to output contact forces to RCFORC for all 2 surface force
          * - :py:attr:`~shltrw`
            - Get or set the Optional shell thickness scale factor for contact with rigid walls. Shell thickness is not considered when SHLTRW=0 (default). SHLTRW=0.5
          * - :py:attr:`~igactc`
            - Get or set the Options to use isogeometric shells for contact detection when


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

    from control_contact import ControlContact

Property detail
---------------

.. py:property:: slsfac
   :type: float


   
   Get or set the Scale factor for sliding interface penalties (default = 0.1)
















   ..
       !! processed by numpydoc !!

.. py:property:: rwpnal
   :type: Optional[float]


   
   Get or set the Scale factor for rigid wall penalties, which treat nodal points interacting with rigid walls, RWPNAL.  The penalties are set so that an absolute value of unity should be optimal; however, this penalty value may be very problem dependent.  If rigid/deformable materials switching is used, this option should be used if the switched materials are interacting with rigid walls.
   In case you have IGA parts in your model, please see Remark 10.
   LT.0.0: All nodes are treated by the penalty method.This is set to - 1.0 for implicit calculations.Since seven(7) variables are stored for each slave node, only the nodes that may interact with the wall should be included in the node list.
   EQ.0.0 : The constraint method is used and nodal points which belong to rigid bodies are not considered.
   GT.0.0 : Rigid bodies nodes are treated by the penalty method and all other nodes are treated by the constraint method.
















   ..
       !! processed by numpydoc !!

.. py:property:: islchk
   :type: int


   
   Get or set the Initial penetration check in contact surfaces.
   EQ.1: no checking,
   EQ.2: full check of initial penetration is performed.
















   ..
       !! processed by numpydoc !!

.. py:property:: shlthk
   :type: int


   
   Get or set the Shell thickness considered in type surface to surface and node to surface type contact options, where options 1 and 2 below activate the new contact algorithms.
   EQ.0: thickness is not considered,
   EQ.1: thickness is considered but rigid bodies are excluded,
   EQ.2: thickness is considered including rigid bodies.
















   ..
       !! processed by numpydoc !!

.. py:property:: penopt
   :type: int


   
   Get or set the Penalty stiffness value option.
   EQ.0: the default is set to 1,
   EQ.1: minimum of master segment and slave node (default for most contact types),
   EQ.2: use master segment stiffness (old way),
   EQ.3: use slave node value,
   EQ.4: use slave node value, area or mass weighted,
   EQ.5: same as 4 but inversely proportional to the shell thickness.
   Options 4 and 5 are recommended for metalforming calculations..
















   ..
       !! processed by numpydoc !!

.. py:property:: thkchg
   :type: int


   
   Get or set the Shell thickness changes considered in single surface contact:
   EQ.0: no consideration (default),
   EQ.1: shell thickness changes are included.
















   ..
       !! processed by numpydoc !!

.. py:property:: orien
   :type: int


   
   Get or set the Optional automatic reorientation of contact interface segments during initialization:
   EQ.0: default is set to 1.
   EQ.1: active for automated (part) input only. Contact surfaces are given by *PART definitions.
   EQ.2: active for manual (segment) and automated (part) input.
   EQ.3: inactive for non-forming contact.
   EQ.4: inactive for froming contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: enmass
   :type: int


   
   Get or set the Treatment of the mass of eroded nodes in contact. This option effects all contact types where nodes are removed after surrounding elements fail. Generally, the removal of eroded nodes makes the calculation more stable; however, in problems where erosion is important the reduction of mass will lead to incorrect results.
   EQ.0: eroding nodes are removed from the calculation.
   EQ.1: eroding nodes of solid elements are retained and continue to be active in contact.
   EQ.2: the eroding nodes of solid and shell elements are retained and continue to be active in contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: usrstr
   :type: int


   
   Get or set the Storage per contact interface for user supplied interface control subroutine.  If zero, no input data is read.
















   ..
       !! processed by numpydoc !!

.. py:property:: usrfrc
   :type: int


   
   Get or set the Storage per contact interface for user supplied interface friction subroutine. If zero, no input data is read.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsbcs
   :type: int


   
   Get or set the Number of cycles between contact searching. Values between 10-100 recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: interm
   :type: int


   
   Get or set the Flag for intermittent searching in old surface to surface contact using the interval specified as NSBCS above:
   EQ.0: off,
   EQ.1: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: xpene
   :type: float


   
   Get or set the Contact surface maximum penetration check multiplier
















   ..
       !! processed by numpydoc !!

.. py:property:: ssthk
   :type: int


   
   Get or set the Flag for using actual shell thickness in single surface contact logic-types 4,13,15 and 26.
   EQ.0: Actual shell thickness is not used in the contacts (default),
   EQ.1: Actual shell thickness is used in the contacts.
















   ..
       !! processed by numpydoc !!

.. py:property:: ecdt
   :type: int


   
   Get or set the Time step size override for eroding contact:
   EQ.0: contact time size may control Dt.
   EQ.1: contact is not considered in Dt determination.
















   ..
       !! processed by numpydoc !!

.. py:property:: tiedprj
   :type: int


   
   Get or set the Bypass projection of slave nodes to master surface in types:
   *CONTACT_TIED_NODES_TO_SURFACE, *CONTACT_TIED_SHELL_EDGE_TO_SURFACE, and, *CONTACT_TIED_SURFACE_TO_SURFACE tied interface options:
   EQ.0: eliminate gaps by projection nodes,
   EQ.1: bypass projection.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfric
   :type: float


   
   Get or set the Default static coefficient of friction (see *PART_CONTACT).
















   ..
       !! processed by numpydoc !!

.. py:property:: dfric
   :type: float


   
   Get or set the Default dynamic coefficient of friction (see *PART_CONTACT).
















   ..
       !! processed by numpydoc !!

.. py:property:: edc
   :type: float


   
   Get or set the Default exponential decay coefficient (see *PART_CONTACT).
















   ..
       !! processed by numpydoc !!

.. py:property:: vfc
   :type: float


   
   Get or set the Default viscous friction coefficient (see *PART_CONTACT).
















   ..
       !! processed by numpydoc !!

.. py:property:: th
   :type: float


   
   Get or set the Default contact thickness (see *PART_CONTACT).
















   ..
       !! processed by numpydoc !!

.. py:property:: th_sf
   :type: float


   
   Get or set the Default thickness scale factor (see *PART_CONTACT).
















   ..
       !! processed by numpydoc !!

.. py:property:: pen_sf
   :type: float


   
   Get or set the Default local penalty scale factor (see *PART_CONTACT).
















   ..
       !! processed by numpydoc !!

.. py:property:: ignore
   :type: int


   
   Get or set the Ignore initial penetrations in the *CONTACT_AUTOMATIC options. This option can also be specified for each interface on the third optional card under the keyword, *CONTACT. The value defined here will be the default.
   EQ.0: Move nodes to eliminate initial penetrations in the model definition.
   EQ.1: Allow initial penetrations to exist by tracking the initial penetrations.
   EQ.2: Allow initial penetrations to exist by tracking the initial penetrations. However, penetration warning messages are printed with the original coordinates and the recommended coordinates of each slave node given.
















   ..
       !! processed by numpydoc !!

.. py:property:: frceng
   :type: int


   
   Get or set the Flag to activate the calculation of frictional sliding energy:
   EQ.0: do not calculate,
   EQ.1: calculation frictional energy in contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: skiprwg
   :type: int


   
   Get or set the Flag not to display stationary rigid wall by default.
   EQ.0:  generate 4 extra nodes and 1 shell element to visulize stationary planar rigid wall.
   EQ.1:  do not generate stationary rigid wall.
















   ..
       !! processed by numpydoc !!

.. py:property:: outseg
   :type: int


   
   Get or set the Flag to output each spot weld slave node and its master segment for contact type: *CONTACT_SPOTWELD into the D3HSP file.
   EQ.0: no, do not write out this information.
   EQ.1: yes, write out this information.
















   ..
       !! processed by numpydoc !!

.. py:property:: spotstp
   :type: int


   
   Get or set the If a spot weld node (related to a *MAT_SPOTWELD beam) cannot be fouind on a master segment, should an error termination occur?
   EQ.0: no, print warning message and continue calculation.
   EQ.1: yes, print error message and terminate.
   EQ.2: no, delete the weld, print a message, and continue,
   EQ.3: no, keep the weld: (This is not recommended as it can lead to instabilities.)
















   ..
       !! processed by numpydoc !!

.. py:property:: spotdel
   :type: int


   
   Get or set the If a spot weld node of a spot weld beam is attached to a shell element, which fails and is deleted, then the attached spot weld beam element is deleted if this flag is on. There is a small cost penalty realted to this option on non-vector processors. On vector processors, however, this option can significantly slow down the calculation if many weld elements fail since the vector lengths are reduced.
   EQ.0: no, do not delete the beam element,
   EQ.1: yes, delete the beam elements when the attached shell fails.
   GT.1: delete the SPR when SPOTDEL nodes are attached to failed elements in the search radius.
















   ..
       !! processed by numpydoc !!

.. py:property:: spothin
   :type: Optional[float]


   
   Get or set the Optional thickness scale factor. If active, define a factor greater than zero, but less than one.
















   ..
       !! processed by numpydoc !!

.. py:property:: isym
   :type: int


   
   Get or set the Symmetry plane default for automatic segment generation when contact is defined by part IDs:
   LT.0:    is a node set on the symmetry boundary, supported and recommended for Mortar contact.
   This will allow for a correct treatment of segments close to the symmetry face/edge. See Remark 8
   EQ.0: Off.
   EQ.1: do not include faces whith normal boundary constraints ( e.g. segments of brick elements on a symmetry plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: nserod
   :type: int


   
   Get or set the Flag to use one way node to surface erosion:
   EQ.0: use two-way algorithm
   EQ.1: use one-way algorithm.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwgaps
   :type: int


   
   Get or set the Flag to add rigid wall gap stiffness, see parameter RWGDTH below.
   EQ.1:  add gap stiffness.
   EQ.2:  do not add gap stiffness
















   ..
       !! processed by numpydoc !!

.. py:property:: rwgdth
   :type: float


   
   Get or set the Death time for gap stiffness. After this time the gap stiffness is no longer added.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwksf
   :type: float


   
   Get or set the Rigid wall penalty scale factor for contact with deformable parts during implicit calculations.  This value is independent of SLSFAC and RWPNAL. If RWKSF is also specified in *RIGIDWALL_PLANAR, the stiffness is scaled by the product of the two values..
















   ..
       !! processed by numpydoc !!

.. py:property:: icov
   :type: int


   
   Get or set the Invokes the covariant formulation of Konyukhov and Schweizerhof in the FORMING contact option.
















   ..
       !! processed by numpydoc !!

.. py:property:: swradf
   :type: float


   
   Get or set the Spot weld radius scale factor for neighbor segment thinning:EQ.0:        Neighbor segments are not thinned(default).GT.0 : The radius of a spot weld is scaled by SWRADF when searching for close neighbor segments to thin.
















   ..
       !! processed by numpydoc !!

.. py:property:: ithoff
   :type: int


   
   Get or set the Thermal contact heat transfer position.
   EQ.0 Heat transferred to mid plane in thick thermal shell elements.
   EQ.1 Heat transferred to outer surface on thick thermal shell elements
















   ..
       !! processed by numpydoc !!

.. py:property:: shledg
   :type: int


   
   Get or set the Flag for assuming edge shape for shells when measuring penetration.
   This is available for segment based contact (see SOFT on *CONTACT)
   EQ.0: Shell edges are assumed round (default),
   EQ.1: Shell edges are assumed square and are flush with the nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: pstiff
   :type: int


   
   Get or set the Flag to choose the method for calculating the penalty stiffness. This is
   available for segment based contact (see SOFT on *CONTACT)
   EQ.0: Based on material density and segment dimensions (default),
   EQ.1: Based on nodal masses.
















   ..
       !! processed by numpydoc !!

.. py:property:: ithcnt
   :type: int


   
   Get or set the Thermal contact heat transfer methodology
   LT.0: conduction evevenly distributed (pre R4)
   EQ.0: default set to 1
   EQ.1: conduction weighted by shape functions, reduced intergration
   EQ.2: conduction weighted by shape functions, full integration
















   ..
       !! processed by numpydoc !!

.. py:property:: tdcnof
   :type: int


   
   Get or set the Tied constraint offset contact update option.
   EQ.0: Update velocities and displacements from accelerations
   EQ.1: Update velocities and acclelerations from displacements. This
   option is recommended only when there are large angle changes
   where the default does not maintain a constant offset to a small
   tolerance. This latter option is not as stable as the default and may
   require additional damping for stability. See *CONTROL_BULK_VISCOSITY and *DAMPING_PART_STIFFNESS.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftall
   :type: int


   
   Get or set the Option to output contact forces to RCFORC for all 2 surface force
   transducers when the force transducer surfaces overlap.
   EQ.0: Output to the first force transducer that matches (default)
   EQ.1: Output to all force transducers that match
















   ..
       !! processed by numpydoc !!

.. py:property:: shltrw
   :type: float


   
   Get or set the Optional shell thickness scale factor for contact with rigid walls. Shell thickness is not considered when SHLTRW=0 (default). SHLTRW=0.5
   will result in an offset of half of shell thickness in contact with rigid walls..
















   ..
       !! processed by numpydoc !!

.. py:property:: igactc
   :type: int


   
   Get or set the Options to use isogeometric shells for contact detection when
   contact involves isogeometric shells:
   EQ.0: contact between interpolated nodes and interpolated shells
   EQ.1: contact between interpolated nodes and isogeometric shells.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'CONTACT'






