





:class:`DatabaseFsi`
====================


.. py:class:: database_fsi.DatabaseFsi(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_FSI keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseFsi

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtout`
            - Get or set the Output interval
          * - :py:attr:`~binary`
            - Get or set the Flag for binary output.  See remarks under "Output Files and Post-Processing" in Appendix O, "LS-DYNA MPP User Guide."
          * - :py:attr:`~dbsfi_id`
            - Get or set the Surface ID (for reference purposes only)
          * - :py:attr:`~sid`
            - Get or set the Set ID .  This Lagrangian SID must be contained in a Lagrangian structure SID defined in a corresponding coupling card, *CONSTRAINED_LAGRANGE_IN_SOLID.
          * - :py:attr:`~stdype`
            - Get or set the Set type:
          * - :py:attr:`~swid`
            - Get or set the Switch ID from a corresponding *ALE_FSI_SWITCH_MMG_ID card.  If defined, the accumulative mass of the switched ALE multi-material group (AMMG) is written out under the pleak parameter in the dbfsi file.
          * - :py:attr:`~convid`
            - Get or set the For airbag application only: Convection ID from a corresponding *LOAD_ALE_CONVECTION_ID card (which computes the heat transfer between inflator gas and the inflator canister).  If defined, the temperature of the Lagrangian part having heat transfer with the gas, and its change in temperature as function of time in the dbfsi file.
          * - :py:attr:`~ndsetid`
            - Get or set the Set ID consisting of the nodes on which the moments of the forces applied on SID are computed.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM


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

    from database_fsi import DatabaseFsi

Property detail
---------------

.. py:property:: dtout
   :type: Optional[float]


   
   Get or set the Output interval
















   ..
       !! processed by numpydoc !!

.. py:property:: binary
   :type: int


   
   Get or set the Flag for binary output.  See remarks under "Output Files and Post-Processing" in Appendix O, "LS-DYNA MPP User Guide."
   EQ.1:   ASCII file is written:  This is the default for shared memory parallel (SMP) LS-DYNA executables.
   EQ.2:   Data written to a binary database binout, which contains data that would otherwise be output to the ASCII file.
   The ASCII file in this case is not created.  This is the default for MPP LS-DYNA executables.
   EQ.3:   ASCII file is written, and the data is also written to the binary database (NOTE: MPP LS-DYNA executables will only produce the binary database).
















   ..
       !! processed by numpydoc !!

.. py:property:: dbsfi_id
   :type: Optional[int]


   
   Get or set the Surface ID (for reference purposes only)
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID .  This Lagrangian SID must be contained in a Lagrangian structure SID defined in a corresponding coupling card, *CONSTRAINED_LAGRANGE_IN_SOLID.
















   ..
       !! processed by numpydoc !!

.. py:property:: stdype
   :type: int


   
   Get or set the Set type:
   EQ.0: Part set,
   EQ.1: Part,
   EQ.2: Segment set.
















   ..
       !! processed by numpydoc !!

.. py:property:: swid
   :type: Optional[int]


   
   Get or set the Switch ID from a corresponding *ALE_FSI_SWITCH_MMG_ID card.  If defined, the accumulative mass of the switched ALE multi-material group (AMMG) is written out under the pleak parameter in the dbfsi file.
















   ..
       !! processed by numpydoc !!

.. py:property:: convid
   :type: Optional[int]


   
   Get or set the For airbag application only: Convection ID from a corresponding *LOAD_ALE_CONVECTION_ID card (which computes the heat transfer between inflator gas and the inflator canister).  If defined, the temperature of the Lagrangian part having heat transfer with the gas, and its change in temperature as function of time in the dbfsi file.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndsetid
   :type: Optional[int]


   
   Get or set the Set ID consisting of the nodes on which the moments of the forces applied on SID are computed.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Coordinate system ID, see *DEFINE_COORDINATE_SYSTEM
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'FSI'






