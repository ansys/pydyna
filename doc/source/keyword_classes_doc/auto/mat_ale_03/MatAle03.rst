





:class:`MatAle03`
=================


.. py:class:: mat_ale_03.MatAle03(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ALE_03 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAle03

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~iadiab`
            - Get or set the This flag (default=0) is used to turn ON/OFF adiabatic compression logics for an ideal gas (remark 5).
          * - :py:attr:`~runiv`
            - Get or set the Universal gas constant in per-mole unit (8.31447 J/(mole*K)).
          * - :py:attr:`~cvmass1`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cvmass2`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cvmass3`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cvmass4`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cvmass5`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cvmass6`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cvmass7`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cvmass8`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass1`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass2`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass3`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass4`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass5`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass6`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass7`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~cpmass8`
            - Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
          * - :py:attr:`~molwt1`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~molwt2`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~molwt3`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~molwt4`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~molwt5`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~molwt6`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~molwt7`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~molwt8`
            - Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
          * - :py:attr:`~cpmole1`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
          * - :py:attr:`~cpmole2`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
          * - :py:attr:`~cpmole3`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
          * - :py:attr:`~cpmole4`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
          * - :py:attr:`~cpmole5`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
          * - :py:attr:`~cpmole6`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
          * - :py:attr:`~cpmole7`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
          * - :py:attr:`~cpmole8`
            - Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable"A" in the equation in remark 2.
          * - :py:attr:`~b1`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~b2`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~b3`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~b4`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~b5`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~b6`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~b7`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~b8`
            - Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
          * - :py:attr:`~c1`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
          * - :py:attr:`~c2`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
          * - :py:attr:`~c3`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
          * - :py:attr:`~c4`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
          * - :py:attr:`~c5`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
          * - :py:attr:`~c6`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
          * - :py:attr:`~c7`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
          * - :py:attr:`~c8`
            - Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
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

    from mat_ale_03 import MatAle03

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: iadiab
   :type: int


   
   Get or set the This flag (default=0) is used to turn ON/OFF adiabatic compression logics for an ideal gas (remark 5).
   EQ.0:  OFF (default)
   EQ.1:  ON
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: runiv
   :type: Optional[float]


   
   Get or set the Universal gas constant in per-mole unit (8.31447 J/(mole*K)).
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass1
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass2
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass3
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass4
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass5
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass6
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass7
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvmass8
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant volume for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass1
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass2
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass3
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass4
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass5
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass6
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass7
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmass8
   :type: Optional[float]


   
   Get or set the If RUNIV is BLANK or zero (method A): Heat capacity at constant pressure for up to eight different gases in per-mass unit.
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt1
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt2
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt3
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt4
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt5
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt6
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt7
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: molwt8
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B):  Molecular weight of each ideal gas in the mixture (mass-unit/mole).
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole1
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole2
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole3
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole4
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole5
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole6
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole7
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable "A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cpmole8
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Heat capacity at constant pressure for up to eight different gases in per-mole unit.  These are nominal heat capacity values typically at STP.  These are denoted by the variable"A" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b3
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b4
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b5
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b6
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b7
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: b8
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): First order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases.  These are denoted by the variable "B" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c7
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: c8
   :type: Optional[float]


   
   Get or set the If RUNIV is nonzero (method B): Second order coefficient for a temperature dependent heat capacity at constant pressure for up to eight different gases. These are denoted by the variable "C" in the equation in remark 2.
















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
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'ALE_03'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





