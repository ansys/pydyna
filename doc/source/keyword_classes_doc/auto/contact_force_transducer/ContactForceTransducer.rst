





:class:`ContactForceTransducer`
===============================


.. py:class:: contact_force_transducer.ContactForceTransducer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_FORCE_TRANSDUCER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactForceTransducer

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfa`
            - Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface (see Setting the Contact Interface). See *SET_SEGMENT, *SET_NODE_OPTION, *PART, *SET_PART or *SET_SHELL_OPTION. For ERODING_SINGLE_SURFACE and ERODING_SURFACE_TO_SURFACE contact types, use either a part ID or a part set ID. For ERODING_NODES_TO_SURFACE contact, use a node set which includes all nodes that may be exposed to contact as element erosion occurs.
          * - :py:attr:`~surfb`
            - Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact (see Setting the Contact Interface).
          * - :py:attr:`~surfatyp`
            - Get or set the The ID type of SURFA:
          * - :py:attr:`~surfbtyp`
            - Get or set the ID type of SURFB:
          * - :py:attr:`~saboxid`
            - Get or set the Include in contact definition only those SURFA nodes/segments within box SABOXID (corresponding to BOXID in *DEFINE_BOX), or if SABOXID is negative, only those SURFA nodes/segments within contact volume |SABOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SABOXID can be used only if SURFATYP is set to 2, 3, or 6, that is, SURFA is a part ID or part set ID. SABOXID is not available for ERODING contact types
          * - :py:attr:`~sbboxid`
            - Get or set the Include in contact definition only those SURFB segments within box SBBOXID (corresponding to BOXID in *DEFINE_BOX), or if SBBOXID is negative, only those SURFB segments within contact volume |SBBOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SBBOXID can be used only if SURFBTYP is set to 2, 3, or 6, that is, SURFB is a part ID or part set ID.  SBBOXID is not available for ERODING contact types.
          * - :py:attr:`~sapr`
            - Get or set the Include the SURFA side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
          * - :py:attr:`~sbpr`
            - Get or set the Include the SURFB side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
          * - :py:attr:`~cid`
            - Get or set the ID keyword option
          * - :py:attr:`~heading`
            - Get or set the Interface descriptor. We suggest using unique descriptions.


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

    from contact_force_transducer import ContactForceTransducer

Property detail
---------------

.. py:property:: surfa
   :type: Optional[int]


   
   Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for specifying the SURFA side of the contact interface (see Setting the Contact Interface). See *SET_SEGMENT, *SET_NODE_OPTION, *PART, *SET_PART or *SET_SHELL_OPTION. For ERODING_SINGLE_SURFACE and ERODING_SURFACE_TO_SURFACE contact types, use either a part ID or a part set ID. For ERODING_NODES_TO_SURFACE contact, use a node set which includes all nodes that may be exposed to contact as element erosion occurs.
   EQ.0:   Includes all parts in the case of single surface contact types
















   ..
       !! processed by numpydoc !!

.. py:property:: surfb
   :type: Optional[int]


   
   Get or set the Segment set ID, node set ID, part set ID, part ID, or shell element set ID for the SURFB side of the contact (see Setting the Contact Interface).
   EQ.0:   SURFB side is not applicable for single surface contact types.
















   ..
       !! processed by numpydoc !!

.. py:property:: surfatyp
   :type: int


   
   Get or set the The ID type of SURFA:
   EQ.0: segment set ID for surface to surface contact,
   EQ.1: shell element set ID for surface to surface contact,
   EQ.2: part set ID,
   EQ.3: part ID,
   EQ.4: node set ID for node to surface contact,
   EQ.5: include all (SURFA field) is ignored,
   EQ.6: part set ID for exempted parts. All non-exempted parts are included in the contact.
   EQ.7:   Branch ID; see *SET_PART_TREE
















   ..
       !! processed by numpydoc !!

.. py:property:: surfbtyp
   :type: int


   
   Get or set the ID type of SURFB:
   EQ.0: segment set ID,
   EQ.1: shell element set ID,
   EQ.2: part set ID,
   EQ.3: part ID,
   EQ.5:Include all ( SURFB Field is ignored).
   EQ.6:   Part set ID for exempted parts.  All non-exempted parts are included in the contact.
   EQ.7:   Branch ID; see *SET_PART_TREE
















   ..
       !! processed by numpydoc !!

.. py:property:: saboxid
   :type: Optional[int]


   
   Get or set the Include in contact definition only those SURFA nodes/segments within box SABOXID (corresponding to BOXID in *DEFINE_BOX), or if SABOXID is negative, only those SURFA nodes/segments within contact volume |SABOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SABOXID can be used only if SURFATYP is set to 2, 3, or 6, that is, SURFA is a part ID or part set ID. SABOXID is not available for ERODING contact types
















   ..
       !! processed by numpydoc !!

.. py:property:: sbboxid
   :type: Optional[int]


   
   Get or set the Include in contact definition only those SURFB segments within box SBBOXID (corresponding to BOXID in *DEFINE_BOX), or if SBBOXID is negative, only those SURFB segments within contact volume |SBBOXID | (corresponding to CVID in *DEFINE_CONTACT_VOLUME). SBBOXID can be used only if SURFBTYP is set to 2, 3, or 6, that is, SURFB is a part ID or part set ID.  SBBOXID is not available for ERODING contact types.
















   ..
       !! processed by numpydoc !!

.. py:property:: sapr
   :type: int


   
   Get or set the Include the SURFA side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
   EQ.0:   Do not include.
   EQ.1 : SURFA side forces included.
   EQ.2 : Same as 1 but also allows for SURFA nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbpr
   :type: int


   
   Get or set the Include the SURFB side in the *DATABASE_NCFORC and the *DATABASE_BINARY_INTFOR interface force files, and optionally in the dynain file for wear:
   EQ.0:   Do not include.
   EQ.1 : SURFB side forces included.
   EQ.2 : Same as 1, but also allows for SURFB nodes to be written as* INITIAL_CONTACT_WEAR to dynain; see NCYC on* INTERFACE_SPRINGBACK_LSDYNA.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the ID keyword option
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the Interface descriptor. We suggest using unique descriptions.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'FORCE_TRANSDUCER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





