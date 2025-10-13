





:class:`DefineConnectionPropertiesAdd`
======================================


.. py:class:: define_connection_properties_add.DefineConnectionPropertiesAdd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CONNECTION_PROPERTIES_ADD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineConnectionPropertiesAdd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~con_id`
            - Get or set the Connection ID of an existing *DEFINE_CONNECTION_PROPERTIES table
          * - :py:attr:`~proprul`
            - Get or set the The failure rule number for this connection
          * - :py:attr:`~areaeq`
            - Get or set the Area equation number for the connection area calculation.
          * - :py:attr:`~dg_typ`
            - Get or set the Damage type
          * - :py:attr:`~moarfl`
            - Get or set the Modeled area flag
          * - :py:attr:`~mid`
            - Get or set the Material ID of the shell material for which properties are defined.
          * - :py:attr:`~sgiy`
            - Get or set the Yield stress to be used in the spot weld element calculation
          * - :py:attr:`~etan`
            - Get or set the Tangent modulus to be used in the spot weld element calculation.
          * - :py:attr:`~dgpr`
            - Get or set the Damage parameter for hyperbolic based damage function.
          * - :py:attr:`~rank`
            - Get or set the Rank value
          * - :py:attr:`~sn`
            - Get or set the Normal strength.
          * - :py:attr:`~sb`
            - Get or set the Bending strength.
          * - :py:attr:`~ss`
            - Get or set the Shear strength
          * - :py:attr:`~exsn`
            - Get or set the Exponent on normal stress term.
          * - :py:attr:`~exsb`
            - Get or set the Exponent on bending stress term.
          * - :py:attr:`~exss`
            - Get or set the Exponent on shear stress term.
          * - :py:attr:`~lcsn`
            - Get or set the Curve ID for normal strength scale factor as a function of strain rate
          * - :py:attr:`~lcsb`
            - Get or set the Curve ID for bending strength scale factor as a function of strain rate.
          * - :py:attr:`~lcss`
            - Get or set the Curve ID for shear strength scale factor as a function of strain rate
          * - :py:attr:`~gfad`
            - Get or set the Fading energy for damage type 3.
          * - :py:attr:`~sclmrr`
            - Get or set the Scaling factor for torsional moment in failure function.
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

    from define_connection_properties_add import DefineConnectionPropertiesAdd

Property detail
---------------

.. py:property:: con_id
   :type: Optional[int]


   
   Get or set the Connection ID of an existing *DEFINE_CONNECTION_PROPERTIES table
















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

.. py:property:: dg_typ
   :type: int


   
   Get or set the Damage type
   EQ.0:  no damage function is used
   EQ.1:  strain based damage
   EQ.2:  failure function based damage
   EQ.3 or 4:  fading energy based damage
















   ..
       !! processed by numpydoc !!

.. py:property:: moarfl
   :type: int


   
   Get or set the Modeled area flag
   EQ.0: Areamodelled goes down with shear (default)
   EQ.1: Areamodelled stays constant
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID of the shell material for which properties are defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: sgiy
   :type: Optional[float]


   
   Get or set the Yield stress to be used in the spot weld element calculation
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Tangent modulus to be used in the spot weld element calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: dgpr
   :type: float


   
   Get or set the Damage parameter for hyperbolic based damage function.
















   ..
       !! processed by numpydoc !!

.. py:property:: rank
   :type: Optional[float]


   
   Get or set the Rank value
















   ..
       !! processed by numpydoc !!

.. py:property:: sn
   :type: Optional[float]


   
   Get or set the Normal strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: sb
   :type: Optional[float]


   
   Get or set the Bending strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: ss
   :type: Optional[float]


   
   Get or set the Shear strength
















   ..
       !! processed by numpydoc !!

.. py:property:: exsn
   :type: Optional[float]


   
   Get or set the Exponent on normal stress term.
















   ..
       !! processed by numpydoc !!

.. py:property:: exsb
   :type: Optional[float]


   
   Get or set the Exponent on bending stress term.
















   ..
       !! processed by numpydoc !!

.. py:property:: exss
   :type: Optional[float]


   
   Get or set the Exponent on shear stress term.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsn
   :type: Optional[int]


   
   Get or set the Curve ID for normal strength scale factor as a function of strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsb
   :type: Optional[int]


   
   Get or set the Curve ID for bending strength scale factor as a function of strain rate.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcss
   :type: Optional[int]


   
   Get or set the Curve ID for shear strength scale factor as a function of strain rate
















   ..
       !! processed by numpydoc !!

.. py:property:: gfad
   :type: Optional[int]


   
   Get or set the Fading energy for damage type 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: sclmrr
   :type: float


   
   Get or set the Scaling factor for torsional moment in failure function.
















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
   :value: 'CONNECTION_PROPERTIES_ADD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





