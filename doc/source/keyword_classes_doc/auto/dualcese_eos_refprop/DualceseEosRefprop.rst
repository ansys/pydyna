





:class:`DualceseEosRefprop`
===========================


.. py:class:: dualcese_eos_refprop.DualceseEosRefprop(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_EOS_REFPROP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseEosRefprop

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the ID for this EOS
          * - :py:attr:`~ncomp`
            - Get or set the Number of components in the fluid composition
          * - :py:attr:`~type`
            - Get or set the The fluid type.
          * - :py:attr:`~phase`
            - Get or set the Phase of the fluid.
          * - :py:attr:`~tabular`
            - Get or set the Type of lookup tables to build for this EOS.
          * - :py:attr:`~mol_fr1`
            - Get or set the Mole fraction
          * - :py:attr:`~mol_fr2`
            - Get or set the Mole fraction
          * - :py:attr:`~mol_fr3`
            - Get or set the Mole fraction
          * - :py:attr:`~mol_fr4`
            - Get or set the Mole fraction
          * - :py:attr:`~mol_fr5`
            - Get or set the Mole fraction
          * - :py:attr:`~mol_fr6`
            - Get or set the Mole fraction
          * - :py:attr:`~mol_fr7`
            - Get or set the Mole fraction
          * - :py:attr:`~mol_fr8`
            - Get or set the Mole fraction
          * - :py:attr:`~n_t`
            - Get or set the Number of temperature values in the tables
          * - :py:attr:`~n_den`
            - Get or set the Number of density values (on a log scale) in the tables
          * - :py:attr:`~den_low`
            - Get or set the Minimum density available in the tables (in model units)
          * - :py:attr:`~den_high`
            - Get or set the Maximum density available in the tables (in model units)
          * - :py:attr:`~t_low`
            - Get or set the Minimum temperature available in the tables (in model units)
          * - :py:attr:`~t_high`
            - Get or set the Maximum temperature available in the tables (in model units)
          * - :py:attr:`~fluidname`
            - Get or set the Name of a fluid that has an EOS in CoolProp. For a list of the supported pure and pseudo-pure fluids


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

    from dualcese_eos_refprop import DualceseEosRefprop

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the ID for this EOS
















   ..
       !! processed by numpydoc !!

.. py:property:: ncomp
   :type: Optional[int]


   
   Get or set the Number of components in the fluid composition
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: Optional[str]


   
   Get or set the The fluid type.
   EQ.PURE:  A single component fluid(default)
   EQ.PSEUDOPURE : A predefined fluid mixture
   amespace
   Q.MIXTURE : A fluid mixture made up of NCOMP components
















   ..
       !! processed by numpydoc !!

.. py:property:: phase
   :type: str


   
   Get or set the Phase of the fluid.
   EQ.GAS: gas phase
   EQ.LIQUID : liquid phase
















   ..
       !! processed by numpydoc !!

.. py:property:: tabular
   :type: Optional[str]


   
   Get or set the Type of lookup tables to build for this EOS.
   EQ.BLANK: Default(no table lookup)
   EQ.P_EIN : Build tables of pressure and internal energy, both as a function of densityand temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr1
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr2
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr3
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr4
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr5
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr6
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr7
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: mol_fr8
   :type: Optional[float]


   
   Get or set the Mole fraction
















   ..
       !! processed by numpydoc !!

.. py:property:: n_t
   :type: Optional[float]


   
   Get or set the Number of temperature values in the tables
















   ..
       !! processed by numpydoc !!

.. py:property:: n_den
   :type: Optional[float]


   
   Get or set the Number of density values (on a log scale) in the tables
















   ..
       !! processed by numpydoc !!

.. py:property:: den_low
   :type: Optional[float]


   
   Get or set the Minimum density available in the tables (in model units)
















   ..
       !! processed by numpydoc !!

.. py:property:: den_high
   :type: Optional[float]


   
   Get or set the Maximum density available in the tables (in model units)
















   ..
       !! processed by numpydoc !!

.. py:property:: t_low
   :type: Optional[float]


   
   Get or set the Minimum temperature available in the tables (in model units)
















   ..
       !! processed by numpydoc !!

.. py:property:: t_high
   :type: Optional[float]


   
   Get or set the Maximum temperature available in the tables (in model units)
















   ..
       !! processed by numpydoc !!

.. py:property:: fluidname
   :type: Optional[str]


   
   Get or set the Name of a fluid that has an EOS in CoolProp. For a list of the supported pure and pseudo-pure fluids
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'EOS_REFPROP'






