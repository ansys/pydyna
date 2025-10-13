





:class:`ControlImplicitSsdDirect`
=================================


.. py:class:: control_implicit_ssd_direct.ControlImplicitSsdDirect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_SSD_DIRECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitSsdDirect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~issflg`
            - Get or set the Complex steady state vibration flag:
          * - :py:attr:`~fmin`
            - Get or set the Minimum frequency in the solution. Units are Hertz.
          * - :py:attr:`~fmax`
            - Get or set the Maximum frequency in the solution. Units are Hertz.
          * - :py:attr:`~nfreq`
            - Get or set the Number of frequencies in the solution.
          * - :py:attr:`~loss`
            - Get or set the Structural loss factor.
          * - :py:attr:`~fspace`
            - Get or set the Solution frequency assignment strategy:
          * - :py:attr:`~fractn`
            - Get or set the Octave fraction. For example, FRACTN = 3 means 1⁄3 octave spacing.


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

    from control_implicit_ssd_direct import ControlImplicitSsdDirect

Property detail
---------------

.. py:property:: issflg
   :type: int


   
   Get or set the Complex steady state vibration flag:
   EQ.0:   Off
   EQ.1 : On.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmin
   :type: Optional[float]


   
   Get or set the Minimum frequency in the solution. Units are Hertz.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: Optional[float]


   
   Get or set the Maximum frequency in the solution. Units are Hertz.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: int


   
   Get or set the Number of frequencies in the solution.
















   ..
       !! processed by numpydoc !!

.. py:property:: loss
   :type: float


   
   Get or set the Structural loss factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: fspace
   :type: float


   
   Get or set the Solution frequency assignment strategy:
   EQ.0:   The frequency is interpolated linearly between FMIN and FMAX.This is the default strategy.
   EQ.1 : The frequency is interpolated on a log scale between FMIN and FMAX, so they are biased to lower frequencies.
   EQ.2 : The frequency is interpolated on a fractional octave scale starting with FMIN.Integer FRACTN is the octave fraction.
   The formula for the active frequency in Hertz is "FACTIVE" = "FMIN" (2.0) ^ (1 / "FRACTN")) ^ (("IFREQ" - 1)).
   IFREQ is the ith frequency in the solution.FMAX is ignored.
   LT.0 : | "FSPACE" | is a load curve ID for assigning active frequencies.The abscissa is frequencies in the solutionand the ordinate is the active frequency in Hertz.FMINand FMAX are ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: fractn
   :type: int


   
   Get or set the Octave fraction. For example, FRACTN = 3 means 1⁄3 octave spacing.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_SSD_DIRECT'






