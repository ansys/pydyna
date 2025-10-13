





:class:`DefineConnectionProperties`
===================================


.. py:class:: define_connection_properties.DefineConnectionProperties(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CONNECTION_PROPERTIES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineConnectionProperties

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~con_id`
            - Get or set the Connection ID, referenced on *MAT_SPOTWELD_DAIMLERCHRYSLER.  Multiple sets of connection data may be used by assigning different connection IDs
          * - :py:attr:`~proprul`
            - Get or set the The failure rule number for this connection
          * - :py:attr:`~areaeq`
            - Get or set the Area equation number for the connection area calculation.
          * - :py:attr:`~dgtyp`
            - Get or set the Damage type
          * - :py:attr:`~moarfl`
            - Get or set the Modeled area flag
          * - :py:attr:`~dsigy`
            - Get or set the Default yield stress for the spot weld element
          * - :py:attr:`~detan`
            - Get or set the Default tangent modulus for the spot weld element.
          * - :py:attr:`~ddg_pr`
            - Get or set the Default damage parameter for hyperbolic based damage function
          * - :py:attr:`~drank`
            - Get or set the Default rank value
          * - :py:attr:`~dsn`
            - Get or set the Default normal strength
          * - :py:attr:`~dsb`
            - Get or set the Default bending strength.
          * - :py:attr:`~dss`
            - Get or set the Default shear strength.
          * - :py:attr:`~dexsn`
            - Get or set the Default exponent on normal stress term.
          * - :py:attr:`~dexsb`
            - Get or set the Default exponent on bending stress term
          * - :py:attr:`~dexss`
            - Get or set the Default exponent on shear stress term.
          * - :py:attr:`~dcsn`
            - Get or set the Default curve ID for normal strength scale factor as a function of strain rate.
          * - :py:attr:`~dlcsb`
            - Get or set the Default curve ID for bending strength scale factor as a function of strain rate.
          * - :py:attr:`~dlcss`
            - Get or set the Default curve ID for shear strength scale factor as a function of strain rate.
          * - :py:attr:`~dgfad`
            - Get or set the Default fading energy for damage type 3.
          * - :py:attr:`~dsclmrr`
            - Get or set the Default scaling factor for torsional moment in failure function.
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

    from define_connection_properties import DefineConnectionProperties

Property detail
---------------

.. py:property:: con_id
   :type: int


   
   Get or set the Connection ID, referenced on *MAT_SPOTWELD_DAIMLERCHRYSLER.  Multiple sets of connection data may be used by assigning different connection IDs
















   ..
       !! processed by numpydoc !!

.. py:property:: proprul
   :type: int


   
   Get or set the The failure rule number for this connection
















   ..
       !! processed by numpydoc !!

.. py:property:: areaeq
   :type: int


   
   Get or set the Area equation number for the connection area calculation.
   EQ.0:   (default) area_true=area_modeled
   EQ.1:   millimeter form;
   EQ.-1:  meter form;
















   ..
       !! processed by numpydoc !!

.. py:property:: dgtyp
   :type: int


   
   Get or set the Damage type
   EQ.0:  no damage function is used
   EQ.1:  strain based damage
   EQ.2:  failure function based damage
   EQ.3 or 4:  fading energy based damage
   EQ.5:   Improved version of DGTYP=4; see Remark 4
















   ..
       !! processed by numpydoc !!

.. py:property:: moarfl
   :type: int


   
   Get or set the Modeled area flag
   EQ.0: Areamodelled goes down with shear (default)
   EQ.1: Areamodelled stays constant
















   ..
       !! processed by numpydoc !!

.. py:property:: dsigy
   :type: Optional[float]


   
   Get or set the Default yield stress for the spot weld element
















   ..
       !! processed by numpydoc !!

.. py:property:: detan
   :type: Optional[float]


   
   Get or set the Default tangent modulus for the spot weld element.
















   ..
       !! processed by numpydoc !!

.. py:property:: ddg_pr
   :type: float


   
   Get or set the Default damage parameter for hyperbolic based damage function
















   ..
       !! processed by numpydoc !!

.. py:property:: drank
   :type: Optional[float]


   
   Get or set the Default rank value
















   ..
       !! processed by numpydoc !!

.. py:property:: dsn
   :type: Optional[float]


   
   Get or set the Default normal strength
















   ..
       !! processed by numpydoc !!

.. py:property:: dsb
   :type: Optional[float]


   
   Get or set the Default bending strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: dss
   :type: Optional[float]


   
   Get or set the Default shear strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: dexsn
   :type: float


   
   Get or set the Default exponent on normal stress term.
















   ..
       !! processed by numpydoc !!

.. py:property:: dexsb
   :type: float


   
   Get or set the Default exponent on bending stress term
















   ..
       !! processed by numpydoc !!

.. py:property:: dexss
   :type: float


   
   Get or set the Default exponent on shear stress term.
















   ..
       !! processed by numpydoc !!

.. py:property:: dcsn
   :type: Optional[int]


   
   Get or set the Default curve ID for normal strength scale factor as a function of strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcsb
   :type: Optional[int]


   
   Get or set the Default curve ID for bending strength scale factor as a function of strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlcss
   :type: Optional[int]


   
   Get or set the Default curve ID for shear strength scale factor as a function of strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: dgfad
   :type: Optional[int]


   
   Get or set the Default fading energy for damage type 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: dsclmrr
   :type: float


   
   Get or set the Default scaling factor for torsional moment in failure function.
















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
   :value: 'CONNECTION_PROPERTIES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





