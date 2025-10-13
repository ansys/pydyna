





:class:`EmEosBurgess`
=====================


.. py:class:: em_eos_burgess.EmEosBurgess(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EOS_BURGESS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEosBurgess

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Id of the EM_EOS (specified in *EM_MAT card).
          * - :py:attr:`~v0`
            - Get or set the reference specific volume V0 (UUS).
          * - :py:attr:`~gamma`
            - Get or set the Reference Gruneisen value r0.(no units).
          * - :py:attr:`~theta`
            - Get or set the reference melting temperature Qm,0 in eV (BUS).
          * - :py:attr:`~lf`
            - Get or set the Latent heat of fusion LF in kJoule/mol (BUS).
          * - :py:attr:`~c1`
            - Get or set the C1 constant (BUS).
          * - :py:attr:`~c2`
            - Get or set the C2 constant (no units).
          * - :py:attr:`~c3`
            - Get or set the C3 constant (no units).
          * - :py:attr:`~c4`
            - Get or set the C4 constant (no units).
          * - :py:attr:`~k`
            - Get or set the Parameter k (no units).
          * - :py:attr:`~expon`
            - Get or set the xponent in equations (2).
          * - :py:attr:`~lgtunit`
            - Get or set the Length units for UUS (relative to meter, i.e. =1.e-3 if UUS in mm).
          * - :py:attr:`~timunit`
            - Get or set the Time units for UUS (relative to seconds).
          * - :py:attr:`~temuni`
            - Get or set the Temperature units
          * - :py:attr:`~adjust`
            - Get or set the ADJUST:


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

    from em_eos_burgess import EmEosBurgess

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Id of the EM_EOS (specified in *EM_MAT card).
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the reference specific volume V0 (UUS).
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: Optional[float]


   
   Get or set the Reference Gruneisen value r0.(no units).
















   ..
       !! processed by numpydoc !!

.. py:property:: theta
   :type: Optional[float]


   
   Get or set the reference melting temperature Qm,0 in eV (BUS).
















   ..
       !! processed by numpydoc !!

.. py:property:: lf
   :type: Optional[float]


   
   Get or set the Latent heat of fusion LF in kJoule/mol (BUS).
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the C1 constant (BUS).
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the C2 constant (no units).
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the C3 constant (no units).
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the C4 constant (no units).
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Parameter k (no units).
















   ..
       !! processed by numpydoc !!

.. py:property:: expon
   :type: Optional[int]


   
   Get or set the xponent in equations (2).
















   ..
       !! processed by numpydoc !!

.. py:property:: lgtunit
   :type: Optional[float]


   
   Get or set the Length units for UUS (relative to meter, i.e. =1.e-3 if UUS in mm).
















   ..
       !! processed by numpydoc !!

.. py:property:: timunit
   :type: Optional[float]


   
   Get or set the Time units for UUS (relative to seconds).
















   ..
       !! processed by numpydoc !!

.. py:property:: temuni
   :type: int


   
   Get or set the Temperature units
   =1: temperature in Celsius
   =2: temperature in Kelvins
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: adjust
   :type: int


   
   Get or set the ADJUST:
   = 0 (default): the conductivity is given by the Burgess formula.
   = 1: The conductivity is adjusted so that it is equal to the conductivity defined in *EM_MAT card   at room temperature
   .
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EOS_BURGESS'






