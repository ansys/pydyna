





:class:`EosJwlAfterburn`
========================


.. py:class:: eos_jwl_afterburn.EosJwlAfterburn(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EOS_JWL_AFTERBURN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EosJwlAfterburn

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID.
          * - :py:attr:`~a`
            - Get or set the Equation of state coefficient, A.
          * - :py:attr:`~b`
            - Get or set the Equation of state coefficient, B.
          * - :py:attr:`~r1`
            - Get or set the Equation of state coefficient, R1.
          * - :py:attr:`~r2`
            - Get or set the Equation of state coefficient, R2.
          * - :py:attr:`~omeg`
            - Get or set the Equation of state coefficient, w.
          * - :py:attr:`~e0`
            - Get or set the Detonation energy per unit volume and initial value for E. See equation in Remarks.
          * - :py:attr:`~vo`
            - Get or set the Initial realtive volume.
          * - :py:attr:`~opt`
            - Get or set the Afterburn option EQ.0.0: No afterburn energy (Standard EOS_JWL)
          * - :py:attr:`~qt`
            - Get or set the Afterburn energy per unit volume for simple afterburn (OPT=1,2).
          * - :py:attr:`~t1`
            - Get or set the Start time of energy addition for simple afterburn.
          * - :py:attr:`~t2`
            - Get or set the End time of energy addition for simple afterburn.
          * - :py:attr:`~q0`
            - Get or set the Afterburn energy per unit volume for Miller's extension (OPT=3).
          * - :py:attr:`~qa`
            - Get or set the Energy release constant a for Miller's extension.
          * - :py:attr:`~qm`
            - Get or set the Energy release exponent m for Miller's extension.
          * - :py:attr:`~qn`
            - Get or set the Pressure exponent n for Miller's extension.
          * - :py:attr:`~conm`
            - Get or set the GT.0.0: Mass conversion factor from model units to calibration units for Miller's extension
          * - :py:attr:`~conl`
            - Get or set the CONM.GT.0.0: Length conversion factor from model units to calibration units for Miller's extension CONM.
          * - :py:attr:`~cont`
            - Get or set the CONM.GT.0.0: Time conversion factor from model units to calibration units for Miller's extension CONM.


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

    from eos_jwl_afterburn import EosJwlAfterburn

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Equation of state coefficient, A.
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the Equation of state coefficient, B.
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: Optional[float]


   
   Get or set the Equation of state coefficient, R1.
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: Optional[float]


   
   Get or set the Equation of state coefficient, R2.
















   ..
       !! processed by numpydoc !!

.. py:property:: omeg
   :type: Optional[float]


   
   Get or set the Equation of state coefficient, w.
















   ..
       !! processed by numpydoc !!

.. py:property:: e0
   :type: Optional[float]


   
   Get or set the Detonation energy per unit volume and initial value for E. See equation in Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: vo
   :type: Optional[float]


   
   Get or set the Initial realtive volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: opt
   :type: float


   
   Get or set the Afterburn option EQ.0.0: No afterburn energy (Standard EOS_JWL)
   EQ.1.0: Constant rate of afterburn energy added between times T1 and T2
   EQ.2.0: Linearly-increasing rate of afterburn energy added between times T1 and T2
   EQ.3.0: Miller's extension for afterburn energy.
















   ..
       !! processed by numpydoc !!

.. py:property:: qt
   :type: Optional[float]


   
   Get or set the Afterburn energy per unit volume for simple afterburn (OPT=1,2).
















   ..
       !! processed by numpydoc !!

.. py:property:: t1
   :type: Optional[float]


   
   Get or set the Start time of energy addition for simple afterburn.
















   ..
       !! processed by numpydoc !!

.. py:property:: t2
   :type: Optional[float]


   
   Get or set the End time of energy addition for simple afterburn.
















   ..
       !! processed by numpydoc !!

.. py:property:: q0
   :type: Optional[float]


   
   Get or set the Afterburn energy per unit volume for Miller's extension (OPT=3).
















   ..
       !! processed by numpydoc !!

.. py:property:: qa
   :type: Optional[float]


   
   Get or set the Energy release constant a for Miller's extension.
















   ..
       !! processed by numpydoc !!

.. py:property:: qm
   :type: float


   
   Get or set the Energy release exponent m for Miller's extension.
















   ..
       !! processed by numpydoc !!

.. py:property:: qn
   :type: float


   
   Get or set the Pressure exponent n for Miller's extension.
















   ..
       !! processed by numpydoc !!

.. py:property:: conm
   :type: float


   
   Get or set the GT.0.0: Mass conversion factor from model units to calibration units for Miller's extension
   LT.0.0: Use predefined factors to convert model units to published
   calibration units of g, cm, μs. Choices for model units are:
   EQ.-1.0: g, mm, ms
   EQ.-2.0: g, cm, ms
   EQ.-3.0: kg, m, s
   EQ.-4.0: kg, mm, ms
   EQ.-5.0: metric ton, mm, s
   EQ.-6.0: lbf-s2/in, in, s
   EQ.-7.0: slug, ft, s.
















   ..
       !! processed by numpydoc !!

.. py:property:: conl
   :type: float


   
   Get or set the CONM.GT.0.0: Length conversion factor from model units to calibration units for Miller's extension CONM.
   LT.0.0: Ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: cont
   :type: float


   
   Get or set the CONM.GT.0.0: Time conversion factor from model units to calibration units for Miller's extension CONM.
   LT.0.0: Ignored.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EOS'


.. py:attribute:: subkeyword
   :value: 'JWL_AFTERBURN'






