





:class:`EosPropellantDeflagration`
==================================


.. py:class:: eos_propellant_deflagration.EosPropellantDeflagration(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_PROPELLANT_DEFLAGRATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosPropellantDeflagration

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state label.
          * - :py:attr:`~a`
            - Get or set the Product JWL coefficient.
          * - :py:attr:`~b`
            - Get or set the Product JWL coefficient.
          * - :py:attr:`~xp1`
            - Get or set the Product JWL coefficient.
          * - :py:attr:`~xp2`
            - Get or set the Product JWL coefficient.
          * - :py:attr:`~frer`
            - Get or set the Unreacted Co-volume.
          * - :py:attr:`~g`
            - Get or set the Product wCv.
          * - :py:attr:`~r1`
            - Get or set the Unreacted JWL coefficient.
          * - :py:attr:`~r2`
            - Get or set the Unreacted JWL coefficient.
          * - :py:attr:`~r3`
            - Get or set the Unreacted wCv.
          * - :py:attr:`~r5`
            - Get or set the Unreacted JWL coefficient.
          * - :py:attr:`~r6`
            - Get or set the Unreacted JWL coefficient.
          * - :py:attr:`~fmxig`
            - Get or set the Initial Fraction Reacted Fo.
          * - :py:attr:`~freq`
            - Get or set the Initial Pressure Po.
          * - :py:attr:`~grow1`
            - Get or set the First burn rate coefficient.
          * - :py:attr:`~em`
            - Get or set the Pressure Exponent (1st term).
          * - :py:attr:`~ar1`
            - Get or set the Exponent on F (1st term).
          * - :py:attr:`~es1`
            - Get or set the Exponent on (1-F) (1st term).
          * - :py:attr:`~cvp`
            - Get or set the Heat capacity products.
          * - :py:attr:`~cvr`
            - Get or set the Heat capacity unreacted.
          * - :py:attr:`~eetal`
            - Get or set the Extra, not presently used.
          * - :py:attr:`~ccrit`
            - Get or set the Product co-volume.
          * - :py:attr:`~enq`
            - Get or set the Heat of Reaction.
          * - :py:attr:`~tmp0`
            - Get or set the Initial Temperature (default = 298°K).
          * - :py:attr:`~grow2`
            - Get or set the Second burn rate coefficient.
          * - :py:attr:`~ar2`
            - Get or set the Exponent on F (2nd term).
          * - :py:attr:`~es2`
            - Get or set the Exponent on (1-F) (2nd term).
          * - :py:attr:`~en`
            - Get or set the Pressure Exponent (2nd term).
          * - :py:attr:`~fmxgr`
            - Get or set the Maximum F for 1st term.
          * - :py:attr:`~fmngr`
            - Get or set the Minimum F for 2nd term.


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

    from eos_propellant_deflagration import EosPropellantDeflagration

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state label.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: float


   
   Get or set the Product JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: float


   
   Get or set the Product JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp1
   :type: float


   
   Get or set the Product JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: xp2
   :type: float


   
   Get or set the Product JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: frer
   :type: float


   
   Get or set the Unreacted Co-volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: float


   
   Get or set the Product wCv.
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: float


   
   Get or set the Unreacted JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: float


   
   Get or set the Unreacted JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: r3
   :type: float


   
   Get or set the Unreacted wCv.
















   ..
       !! processed by numpydoc !!

.. py:property:: r5
   :type: float


   
   Get or set the Unreacted JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: r6
   :type: float


   
   Get or set the Unreacted JWL coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmxig
   :type: float


   
   Get or set the Initial Fraction Reacted Fo.
















   ..
       !! processed by numpydoc !!

.. py:property:: freq
   :type: float


   
   Get or set the Initial Pressure Po.
















   ..
       !! processed by numpydoc !!

.. py:property:: grow1
   :type: float


   
   Get or set the First burn rate coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: em
   :type: float


   
   Get or set the Pressure Exponent (1st term).
















   ..
       !! processed by numpydoc !!

.. py:property:: ar1
   :type: float


   
   Get or set the Exponent on F (1st term).
















   ..
       !! processed by numpydoc !!

.. py:property:: es1
   :type: float


   
   Get or set the Exponent on (1-F) (1st term).
















   ..
       !! processed by numpydoc !!

.. py:property:: cvp
   :type: float


   
   Get or set the Heat capacity products.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvr
   :type: float


   
   Get or set the Heat capacity unreacted.
















   ..
       !! processed by numpydoc !!

.. py:property:: eetal
   :type: float


   
   Get or set the Extra, not presently used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ccrit
   :type: float


   
   Get or set the Product co-volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: enq
   :type: float


   
   Get or set the Heat of Reaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: tmp0
   :type: float


   
   Get or set the Initial Temperature (default = 298°K).
















   ..
       !! processed by numpydoc !!

.. py:property:: grow2
   :type: float


   
   Get or set the Second burn rate coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: ar2
   :type: float


   
   Get or set the Exponent on F (2nd term).
















   ..
       !! processed by numpydoc !!

.. py:property:: es2
   :type: float


   
   Get or set the Exponent on (1-F) (2nd term).
















   ..
       !! processed by numpydoc !!

.. py:property:: en
   :type: float


   
   Get or set the Pressure Exponent (2nd term).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmxgr
   :type: float


   
   Get or set the Maximum F for 1st term.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmngr
   :type: float


   
   Get or set the Minimum F for 2nd term.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'PROPELLANT_DEFLAGRATION'






