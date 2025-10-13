





:class:`DefineAlebagHole`
=========================


.. py:class:: define_alebag_hole.DefineAlebagHole(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ALEBAG_HOLE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineAlebagHole

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~holeid`
            - Get or set the Bag hole definition ID, referred in *AIRBAG_ADVANCED_ALE.
          * - :py:attr:`~sid`
            - Get or set the Set of Lagrange shell elements to interact with inflator gas
          * - :py:attr:`~sidtype`
            - Get or set the Type of SID
          * - :py:attr:`~nquad`
            - Get or set the The number of flow-sensor points to be distributed over each monitoring surface/segment.  There should be enough sensor points distributed to monitor the flow in each ALE element intersected by this monitoring surface (default=1).
          * - :py:attr:`~xoff`
            - Get or set the Offset distance away from the monitoring surface, beyond which the AMMGID is switched.  The direction of XOFF depends on the normal vector of the monitoring segment.  This offset distance should be at least 1 ALE element width away from, and beyond the monitoring interface (default=0.0).
          * - :py:attr:`~nfold`
            - Get or set the Flag for checking folding logic (default=0=off).  If NFOLD=1=on, then LS-DYNA will check if the monitoring segment is in the fold, applicable to airbag.  If the monitoring segment is still located within a folded (shell) region, then no switching is allowed yet until it has unfolded.
          * - :py:attr:`~xclen`
            - Get or set the This is an absolute distance for distributing the flow sensor points over over the ALE elements.  To make sure that at least 1 sensor point, defined on each Lagrangian segment, is present in each ALE element to track the flow of an AMMG, XLEN may be estimated as roughly half the length of the smallest ALE element in the mesh.  This overwrites the NQUAD distribution of sensor points (default=0.0).
          * - :py:attr:`~int_ext`
            - Get or set the
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

    from define_alebag_hole import DefineAlebagHole

Property detail
---------------

.. py:property:: holeid
   :type: Optional[int]


   
   Get or set the Bag hole definition ID, referred in *AIRBAG_ADVANCED_ALE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set of Lagrange shell elements to interact with inflator gas
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtype
   :type: int


   
   Get or set the Type of SID
   EQ:'PSET' or '0' for set of parts
   EQ:'PART' or '1' for part
















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

.. py:property:: nfold
   :type: int


   
   Get or set the Flag for checking folding logic (default=0=off).  If NFOLD=1=on, then LS-DYNA will check if the monitoring segment is in the fold, applicable to airbag.  If the monitoring segment is still located within a folded (shell) region, then no switching is allowed yet until it has unfolded.
















   ..
       !! processed by numpydoc !!

.. py:property:: xclen
   :type: float


   
   Get or set the This is an absolute distance for distributing the flow sensor points over over the ALE elements.  To make sure that at least 1 sensor point, defined on each Lagrangian segment, is present in each ALE element to track the flow of an AMMG, XLEN may be estimated as roughly half the length of the smallest ALE element in the mesh.  This overwrites the NQUAD distribution of sensor points (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: int_ext
   :type: int


   
   Get or set the 
   EQ: 0 'EXT' if the hole is an external hole
   EQ: 1 'INT' if the hole is an internal hole
















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
   :value: 'ALEBAG_HOLE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





