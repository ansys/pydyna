





:class:`DefineFormingContact`
=============================


.. py:class:: define_forming_contact.DefineFormingContact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FORMING_CONTACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFormingContact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ips`
            - Get or set the Part ID of a slave sliding member, typically a deformable sheet metal blank.
          * - :py:attr:`~ipm`
            - Get or set the Part ID of a master sliding member, typically a tool or die defined as a rigid body.
          * - :py:attr:`~fs`
            - Get or set the Coulomb friction coefficient.
          * - :py:attr:`~oneway`
            - Get or set the Define FORMING contact type:
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

    from define_forming_contact import DefineFormingContact

Property detail
---------------

.. py:property:: ips
   :type: Optional[int]


   
   Get or set the Part ID of a slave sliding member, typically a deformable sheet metal blank.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipm
   :type: Optional[int]


   
   Get or set the Part ID of a master sliding member, typically a tool or die defined as a rigid body.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: Optional[float]


   
   Get or set the Coulomb friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: oneway
   :type: int


   
   Get or set the Define FORMING contact type:
   EQ.0:   The contact is FORMING_ONE_WAY_SURFACE_TO_ SURFACE.
   EQ.1:   The contact is FORMING_ SURFACE_TO_ SURFACE.
















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
   :value: 'FORMING_CONTACT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





