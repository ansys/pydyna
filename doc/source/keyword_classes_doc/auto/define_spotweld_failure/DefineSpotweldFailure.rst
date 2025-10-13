





:class:`DefineSpotweldFailure`
==============================


.. py:class:: define_spotweld_failure.DefineSpotweldFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPOTWELD_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSpotweldFailure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identification number of data set, input as FVAL on *MAT_SPOTWELD.
          * - :py:attr:`~tflag`
            - Get or set the Thickness flag for nominal stress calculation
          * - :py:attr:`~dc1`
            - Get or set the Dynamic coefficient C1.
          * - :py:attr:`~dc2`
            - Get or set the Dynamic coefficient C2.
          * - :py:attr:`~dc3`
            - Get or set the Dynamic coefficient C3.
          * - :py:attr:`~dc4`
            - Get or set the Dynamic coefficient C4.
          * - :py:attr:`~exn`
            - Get or set the Exponent on the normal term.
          * - :py:attr:`~exs`
            - Get or set the Exponent on the shear term.
          * - :py:attr:`~navg`
            - Get or set the Number of points in the time average of the load rates.
          * - :py:attr:`~d_sn`
            - Get or set the Reference value of the static normal strength.
          * - :py:attr:`~d_ss`
            - Get or set the Reference value of the static shear strength.
          * - :py:attr:`~r_sult`
            - Get or set the Reference ultimate strength .
          * - :py:attr:`~tscale`
            - Get or set the Scale factor for thickness used in nominal stress calculations.
          * - :py:attr:`~mid`
            - Get or set the Material ID number of welded shell material.
          * - :py:attr:`~sn`
            - Get or set the Static normal strength of material MID.
          * - :py:attr:`~ss`
            - Get or set the Static shear strength of material MID.
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

    from define_spotweld_failure import DefineSpotweldFailure

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identification number of data set, input as FVAL on *MAT_SPOTWELD.
















   ..
       !! processed by numpydoc !!

.. py:property:: tflag
   :type: int


   
   Get or set the Thickness flag for nominal stress calculation
   EQ.0:   Use minimum sheet thickness
   EQ.1:   Use average sheet thickness
   EQ.2    Use maximum sheet thickness
   EQ.3:   Use sum of sheet thicknesses.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc1
   :type: float


   
   Get or set the Dynamic coefficient C1.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc2
   :type: float


   
   Get or set the Dynamic coefficient C2.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc3
   :type: float


   
   Get or set the Dynamic coefficient C3.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc4
   :type: float


   
   Get or set the Dynamic coefficient C4.
















   ..
       !! processed by numpydoc !!

.. py:property:: exn
   :type: float


   
   Get or set the Exponent on the normal term.
















   ..
       !! processed by numpydoc !!

.. py:property:: exs
   :type: float


   
   Get or set the Exponent on the shear term.
















   ..
       !! processed by numpydoc !!

.. py:property:: navg
   :type: int


   
   Get or set the Number of points in the time average of the load rates.
















   ..
       !! processed by numpydoc !!

.. py:property:: d_sn
   :type: float


   
   Get or set the Reference value of the static normal strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: d_ss
   :type: float


   
   Get or set the Reference value of the static shear strength.
















   ..
       !! processed by numpydoc !!

.. py:property:: r_sult
   :type: float


   
   Get or set the Reference ultimate strength .
















   ..
       !! processed by numpydoc !!

.. py:property:: tscale
   :type: float


   
   Get or set the Scale factor for thickness used in nominal stress calculations.
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID number of welded shell material.
















   ..
       !! processed by numpydoc !!

.. py:property:: sn
   :type: Optional[float]


   
   Get or set the Static normal strength of material MID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ss
   :type: Optional[float]


   
   Get or set the Static shear strength of material MID.
















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
   :value: 'SPOTWELD_FAILURE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





