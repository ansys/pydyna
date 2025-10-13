





:class:`MatTrip`
================


.. py:class:: mat_trip.MatTrip(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_TRIP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatTrip

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~cp`
            - Get or set the Adiabatic temperature calculation option:
          * - :py:attr:`~t0`
            - Get or set the Initial temperature T0 of the material if adiabatic temperature calculation is enabled
          * - :py:attr:`~tref`
            - Get or set the Reference temperature for output of the yield stress as history variable 1.
          * - :py:attr:`~ta0`
            - Get or set the Reference temperature TA0, the absolute zero for the used temperature scale, e.g. -273.15 if the Celsius scale is used and 0.0 if the Kelvin scale is used.
          * - :py:attr:`~a`
            - Get or set the Martensite rate equation parameter A, .
          * - :py:attr:`~b`
            - Get or set the Martensite rate equation parameter B,
          * - :py:attr:`~c`
            - Get or set the Martensite rate equation parameter C,
          * - :py:attr:`~d`
            - Get or set the Martensite rate equation parameter D, .
          * - :py:attr:`~p`
            - Get or set the Martensite rate equation parameter p,
          * - :py:attr:`~q`
            - Get or set the Martensite rate equation parameter q,
          * - :py:attr:`~e0mart`
            - Get or set the Martensite rate equation parameter Eomart,
          * - :py:attr:`~vm0`
            - Get or set the The initial volume fraction of martensite 0.0<Vm0<1.0 may be initialised using two different methods:
          * - :py:attr:`~ahs`
            - Get or set the Hardening law parameter AHS.
          * - :py:attr:`~bhs`
            - Get or set the Hardening law parameter BHS
          * - :py:attr:`~m`
            - Get or set the Hardening law parameter M
          * - :py:attr:`~n`
            - Get or set the Hardening law parameter N.
          * - :py:attr:`~eps0`
            - Get or set the Hardening law parameter EPSO
          * - :py:attr:`~hmart`
            - Get or set the Hardening law parameter HMART
          * - :py:attr:`~k1`
            - Get or set the Hardening law parameter K1
          * - :py:attr:`~k2`
            - Get or set the Hardening law parameter K2
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

    from mat_trip import MatTrip

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Adiabatic temperature calculation option:
   EQ. 0.0 Adiabatic temperature calculation is disabled.
   GT. 0.0 CP is the specific heat Cp. Adiabatic temperature calculation is enabled
















   ..
       !! processed by numpydoc !!

.. py:property:: t0
   :type: Optional[float]


   
   Get or set the Initial temperature T0 of the material if adiabatic temperature calculation is enabled
















   ..
       !! processed by numpydoc !!

.. py:property:: tref
   :type: Optional[float]


   
   Get or set the Reference temperature for output of the yield stress as history variable 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ta0
   :type: Optional[float]


   
   Get or set the Reference temperature TA0, the absolute zero for the used temperature scale, e.g. -273.15 if the Celsius scale is used and 0.0 if the Kelvin scale is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter A, .
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter B,
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter C,
















   ..
       !! processed by numpydoc !!

.. py:property:: d
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter D, .
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter p,
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter q,
















   ..
       !! processed by numpydoc !!

.. py:property:: e0mart
   :type: Optional[float]


   
   Get or set the Martensite rate equation parameter Eomart,
















   ..
       !! processed by numpydoc !!

.. py:property:: vm0
   :type: Optional[float]


   
   Get or set the The initial volume fraction of martensite 0.0<Vm0<1.0 may be initialised using two different methods:
   GT.0.0: Vm0 is set to VM0.
   LT.0.0: Can be used only when there are initial plastic strains  p present, e.g. when using *INITIAL_STRESS_SHELL. The absolute value of VM0 is then the load curve ID for a function f that sets . The function f must be a monotonically nondecreasing function of
















   ..
       !! processed by numpydoc !!

.. py:property:: ahs
   :type: Optional[float]


   
   Get or set the Hardening law parameter AHS.
















   ..
       !! processed by numpydoc !!

.. py:property:: bhs
   :type: Optional[float]


   
   Get or set the Hardening law parameter BHS
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Hardening law parameter M
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Hardening law parameter N.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps0
   :type: Optional[float]


   
   Get or set the Hardening law parameter EPSO
















   ..
       !! processed by numpydoc !!

.. py:property:: hmart
   :type: Optional[float]


   
   Get or set the Hardening law parameter HMART
















   ..
       !! processed by numpydoc !!

.. py:property:: k1
   :type: Optional[float]


   
   Get or set the Hardening law parameter K1
















   ..
       !! processed by numpydoc !!

.. py:property:: k2
   :type: Optional[float]


   
   Get or set the Hardening law parameter K2
















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
   :value: 'TRIP'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





