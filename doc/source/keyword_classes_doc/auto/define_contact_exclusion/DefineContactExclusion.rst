





:class:`DefineContactExclusion`
===============================


.. py:class:: define_contact_exclusion.DefineContactExclusion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CONTACT_EXCLUSION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineContactExclusion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Exclusion ID.
          * - :py:attr:`~title`
            - Get or set the Exclusion Title.
          * - :py:attr:`~target`
            - Get or set the Contact interface from which tied nodes are to be excluded. This
          * - :py:attr:`~c1`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c2`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c3`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c4`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c5`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c6`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c7`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c8`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c9`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c10`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c11`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c12`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c13`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c14`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
          * - :py:attr:`~c15`
            - Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional


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

    from define_contact_exclusion import DefineContactExclusion

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Exclusion ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Exclusion Title.
















   ..
       !! processed by numpydoc !!

.. py:property:: target
   :type: Optional[int]


   
   Get or set the Contact interface from which tied nodes are to be excluded. This
   must be the ID of a SINGLE_SURFACE, NODE_TO_SURFACE, or
   SURFACE_TO_SURFACE contact with SOFT 2..
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c7
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c8
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c9
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c10
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c11
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c12
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c13
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c14
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!

.. py:property:: c15
   :type: Optional[int]


   
   Get or set the The IDs of TIED contacts: 7 on the first card and 8 per additional
   card for as many cards as necessary.
   Any node which is a slave node in one of these interfaces, and is in
   fact tied, will not be processed (as a slave node) in the Target interface.
   Note that if a node is excluded from the Target by this mechanism,
   contact forces may still be applied to the node due to any slave or
   master nodes impacting the contact segments of which it is a part
   (no contact SEGMENTS are deleted, only contact NODES).
   If the Target contact is of type SURFACE_TO_SURFACE, any tied
   slave nodes are deleted from both the slave side (for the normal
   treatment) and the master side (for the symmetric treatment).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CONTACT_EXCLUSION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





