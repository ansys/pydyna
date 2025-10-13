





:class:`AleFsiProjection`
=========================


.. py:class:: ale_fsi_projection.AleFsiProjection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_FSI_PROJECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleFsiProjection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lagsid`
            - Get or set the A set ID defining lagrangian part(s) for this coupling(structures).
          * - :py:attr:`~alesid`
            - Get or set the A set ID defining the ALE part(s) for this coupling(fluids).
          * - :py:attr:`~lsidtyp`
            - Get or set the lagrangian Set ID TYPE.
          * - :py:attr:`~asidtyp`
            - Get or set the ALE Set ID type.
          * - :py:attr:`~smmgid`
            - Get or set the A set ID referring to a group of one or more ALE-Multi-Material-Group (AMMG) IDs which represents the ALE materials interacting with the Lagrangian structure.  This SMMGID is a set ID defined by *SET_MULTI-MATERIAL_GROUP_LIST.
          * - :py:attr:`~icorrec`
            - Get or set the Advection error correction method (See Remark 1).
          * - :py:attr:`~inorm`
            - Get or set the Type of coupling.
          * - :py:attr:`~birth`
            - Get or set the Start time for coupling
          * - :py:attr:`~death`
            - Get or set the End time for coupling


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

    from ale_fsi_projection import AleFsiProjection

Property detail
---------------

.. py:property:: lagsid
   :type: Optional[int]


   
   Get or set the A set ID defining lagrangian part(s) for this coupling(structures).
















   ..
       !! processed by numpydoc !!

.. py:property:: alesid
   :type: Optional[int]


   
   Get or set the A set ID defining the ALE part(s) for this coupling(fluids).
















   ..
       !! processed by numpydoc !!

.. py:property:: lsidtyp
   :type: int


   
   Get or set the lagrangian Set ID TYPE.
   EQ.0: Part set ID (PSID) (default).
   EQ.1: Part ID (PID)
















   ..
       !! processed by numpydoc !!

.. py:property:: asidtyp
   :type: int


   
   Get or set the ALE Set ID type.
   EQ.0: Part set ID (PSID) (default).
   EQ.1: Part ID (PID)
















   ..
       !! processed by numpydoc !!

.. py:property:: smmgid
   :type: Optional[int]


   
   Get or set the A set ID referring to a group of one or more ALE-Multi-Material-Group (AMMG) IDs which represents the ALE materials interacting with the Lagrangian structure.  This SMMGID is a set ID defined by *SET_MULTI-MATERIAL_GROUP_LIST.
















   ..
       !! processed by numpydoc !!

.. py:property:: icorrec
   :type: Optional[int]


   
   Get or set the Advection error correction method (See Remark 1).
   EQ.1: ALE mass is conserved.  Leaked mass is moved,
   EQ.2: ALE mass is almost conserved,
   EQ.3: No correction performed (default).  ALE mass is conserved.  Some leakage may occur.  This may be the best solution.
















   ..
       !! processed by numpydoc !!

.. py:property:: inorm
   :type: Optional[int]


   
   Get or set the Type of coupling.
   EQ.0: Couple in all directions,
   EQ.1: Couple in compression and tension (free sliding),
   EQ.2: Couple in compression only (free sliding).  This choice requires ICORREC=3.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Start time for coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the End time for coupling
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'FSI_PROJECTION'






