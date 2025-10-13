





:class:`DatabaseCrossSectionSet`
================================


.. py:class:: database_cross_section_set.DatabaseCrossSectionSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_CROSS_SECTION_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseCrossSectionSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~csid`
            - Get or set the Optional ID for cross section. If not specified cross section ID is taken to be the cross section order in the input deck.
          * - :py:attr:`~title`
            - Get or set the Crowss section descriptor. It is suggested that unique descriptions be used.
          * - :py:attr:`~nsid`
            - Get or set the Nodal set ID, see *SET_NODE_option.
          * - :py:attr:`~hsid`
            - Get or set the Solid element set ID, see *SET_SOLID.
          * - :py:attr:`~bsid`
            - Get or set the Beam element set ID, see *SET_BEAM.
          * - :py:attr:`~ssid`
            - Get or set the Shell element set ID, see *SET_SHELL_option.
          * - :py:attr:`~tsid`
            - Get or set the Thick shell element set ID, see *SET_TSHELL.
          * - :py:attr:`~dsid`
            - Get or set the Discrete element set ID, see *SET_DISCRETE.
          * - :py:attr:`~id`
            - Get or set the Rigid body (see *MAT_RIGID, type 20) or accelerometer ID (see *ELEMENT_ SEATBELT_ACCELEROMETER). The force resultants are output in the updated local system of the rigid body or accelerometer.
          * - :py:attr:`~itype`
            - Get or set the Flag for local system type:


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

    from database_cross_section_set import DatabaseCrossSectionSet

Property detail
---------------

.. py:property:: csid
   :type: Optional[int]


   
   Get or set the Optional ID for cross section. If not specified cross section ID is taken to be the cross section order in the input deck.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Crowss section descriptor. It is suggested that unique descriptions be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: int


   
   Get or set the Nodal set ID, see *SET_NODE_option.
















   ..
       !! processed by numpydoc !!

.. py:property:: hsid
   :type: int


   
   Get or set the Solid element set ID, see *SET_SOLID.
















   ..
       !! processed by numpydoc !!

.. py:property:: bsid
   :type: int


   
   Get or set the Beam element set ID, see *SET_BEAM.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: int


   
   Get or set the Shell element set ID, see *SET_SHELL_option.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsid
   :type: int


   
   Get or set the Thick shell element set ID, see *SET_TSHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsid
   :type: int


   
   Get or set the Discrete element set ID, see *SET_DISCRETE.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Rigid body (see *MAT_RIGID, type 20) or accelerometer ID (see *ELEMENT_ SEATBELT_ACCELEROMETER). The force resultants are output in the updated local system of the rigid body or accelerometer.
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the Flag for local system type:
   EQ. 0: rigid body (default),
   EQ. 1: accelerometer.
   EQ. 2: coordinate ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'CROSS_SECTION_SET'






