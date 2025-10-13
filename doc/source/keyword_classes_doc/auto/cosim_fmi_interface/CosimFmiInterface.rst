





:class:`CosimFmiInterface`
==========================


.. py:class:: cosim_fmi_interface.CosimFmiInterface(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA COSIM_FMI_INTERFACE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CosimFmiInterface

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~appid`
            - Get or set the FMU (functional mock-up unit) identification. Each FMU must have a unique APPID
          * - :py:attr:`~impexp`
            - Get or set the Import/export flag:
          * - :py:attr:`~regtyp`
            - Get or set the Type of interface region:
          * - :py:attr:`~regid`
            - Get or set the ID of the corresponding region type in LS-DYNA. If region ID is negative for a node set and FIELD = FX, FY or FZ, all nodes within this set share the same imported force value from the FMU. Otherwise, a distinct force value will be imported for each node.
          * - :py:attr:`~field`
            - Get or set the Field data to be imported or exported, depending on the region type. See Remark 4 for a full list of fields
          * - :py:attr:`~winit`
            - Get or set the Initial value
          * - :py:attr:`~ratio`
            - Get or set the Scale factor applied during co-simulation (not FMU generation). When LS-DYNA exports variables, the actual value to be sent is RATIO ?LS-DYNA value. When LS-DYNA imports data from FMU, the actual value received is the FMU value / RATIO.
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID (see *DEFINE_?COORDINATE).
          * - :py:attr:`~ref`
            - Get or set the Control how the coordinate system is used for the variable output when CID? > 0 (see Remark 5):


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

    from cosim_fmi_interface import CosimFmiInterface

Property detail
---------------

.. py:property:: appid
   :type: Optional[str]


   
   Get or set the FMU (functional mock-up unit) identification. Each FMU must have a unique APPID
















   ..
       !! processed by numpydoc !!

.. py:property:: impexp
   :type: str


   
   Get or set the Import/export flag:
   EQ.IMP: Variables are to be imported into LS - DYNA.
   EQ.EXP : Variables are to be exported from LS - DYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: regtyp
   :type: str


   
   Get or set the Type of interface region:
   EQ.NODE:        Single node
   EQ.NSET : Node set
   EQ.SSET : Segment set
   EQ.PART : Rigid part
   EQ.FUNC : User defined curve function to be exported only.See Remark 1 and Example 2.
   EQ.CURV : User defined curve to be imported only.See Remark 2 and Example 3.
   EQ.SESW : Sense switch to be imported only.REGID is neglected with the sense switch specified in FIELD.See Remark 3 and Example 4.
   EQ.BAG: Control-volume airbag.
















   ..
       !! processed by numpydoc !!

.. py:property:: regid
   :type: Optional[int]


   
   Get or set the ID of the corresponding region type in LS-DYNA. If region ID is negative for a node set and FIELD = FX, FY or FZ, all nodes within this set share the same imported force value from the FMU. Otherwise, a distinct force value will be imported for each node.
















   ..
       !! processed by numpydoc !!

.. py:property:: field
   :type: Optional[str]


   
   Get or set the Field data to be imported or exported, depending on the region type. See Remark 4 for a full list of fields
















   ..
       !! processed by numpydoc !!

.. py:property:: winit
   :type: Optional[float]


   
   Get or set the Initial value
















   ..
       !! processed by numpydoc !!

.. py:property:: ratio
   :type: Optional[float]


   
   Get or set the Scale factor applied during co-simulation (not FMU generation). When LS-DYNA exports variables, the actual value to be sent is RATIO ?LS-DYNA value. When LS-DYNA imports data from FMU, the actual value received is the FMU value / RATIO.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID (see *DEFINE_?COORDINATE).
   EQ.0:   global(default)
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: int


   
   Get or set the Control how the coordinate system is used for the variable output when CID? > 0 (see Remark 5):
   EQ.0:   Variable output is in the local system fixed from the beginning.Note that you should set FLAG = 0 in * DEFINE_COORDINATE_NODES.
   EQ.1 : Variable output is projected onto the moving local system.
   EQ.2 : Variable output is the projection of the nodal translation motion relative to node N1 of the local coordinate system, COOR.Double precision LS - DYNA is recommended with REF = 2
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'COSIM'


.. py:attribute:: subkeyword
   :value: 'FMI_INTERFACE'






