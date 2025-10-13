





:class:`EmCircuit`
==================


.. py:class:: em_circuit.EmCircuit(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CIRCUIT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmCircuit

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~circid`
            - Get or set the Circuit ID.
          * - :py:attr:`~circtyp`
            - Get or set the Circuit type:
          * - :py:attr:`~lcid`
            - Get or set the Load curve Id for CIRCTYP=1,2,21 or 22.
          * - :py:attr:`~r_f`
            - Get or set the Value of the circuit resistance for CIRCTYP.EQ.3.
          * - :py:attr:`~l_a`
            - Get or set the Value of the circuit inductance for CIRCTYP.EQ.3
          * - :py:attr:`~c_t0`
            - Get or set the Value of the circuit capacity for CIRCTYP.EQ.3
          * - :py:attr:`~v0`
            - Get or set the Value of the circuit initial voltage for CIRCTYP.EQ.3
          * - :py:attr:`~t0`
            - Get or set the Starting time for CIRCTYPE = 3. Default is at the beginning of the run.
          * - :py:attr:`~sidcurr`
            - Get or set the Segment set ID for the current. It uses the orientation given by the
          * - :py:attr:`~sidvin`
            - Get or set the Segment set ID for input voltage or input current when CIRCTYP.EQ.2/3/12/22 or CIRCTYP.EQ.1/11/21 respectively. It is considered to be oriented as going into the structural mesh, irrespective of the orientation of the segment.
          * - :py:attr:`~sidvout`
            - Get or set the Segment set ID for output voltage or output current when CIRCTYP.EQ.2/3/12/22 or CIRCTYP.EQ.1/11/21 repecitively. It is considered to be oriented as going out of the structural mesh, irrespective of the orientation of the segment
          * - :py:attr:`~partid`
            - Get or set the Part ID associated to the Circuit. It can be any part ID associated to the circuit.


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

    from em_circuit import EmCircuit

Property detail
---------------

.. py:property:: circid
   :type: Optional[int]


   
   Get or set the Circuit ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: circtyp
   :type: int


   
   Get or set the Circuit type:
   EQ.1: Imposed current vs time defined by a load curve.
   EQ.2: Imposed voltage vs time defined by a load curve.
   EQ.3: R,L,C,V0 circuit.
   EQ.11: Imposed current defined by an amplitude A, frequency F and initial time t0 : I = Asin[2*PI*F*(t-t0)].
   EQ.12: Imposed voltage defined by an amplitude A, frequency F and initial time t0 : V = Asin[2*PI*F*(t-t0)].
   EQ.21: Imposed current defined by a load curve over one period and a frequency F.
   EQ.22: Imposed voltage defined by a load curve over one period and a frequency F.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve Id for CIRCTYP=1,2,21 or 22.
















   ..
       !! processed by numpydoc !!

.. py:property:: r_f
   :type: Optional[float]


   
   Get or set the Value of the circuit resistance for CIRCTYP.EQ.3.
   Value of the Frequency for CIRCTYP.EQ.11,12,21 or 22.
















   ..
       !! processed by numpydoc !!

.. py:property:: l_a
   :type: Optional[float]


   
   Get or set the Value of the circuit inductance for CIRCTYP.EQ.3
   Value of the Amplitude for CIRCTYP.EQ.11 or 12
















   ..
       !! processed by numpydoc !!

.. py:property:: c_t0
   :type: Optional[float]


   
   Get or set the Value of the circuit capacity for CIRCTYP.EQ.3
   Value of the initial time t0 for CIRCTYP.EQ.11 or 12
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Value of the circuit initial voltage for CIRCTYP.EQ.3
















   ..
       !! processed by numpydoc !!

.. py:property:: t0
   :type: float


   
   Get or set the Starting time for CIRCTYPE = 3. Default is at the beginning of the run.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidcurr
   :type: Optional[int]


   
   Get or set the Segment set ID for the current. It uses the orientation given by the
   normal of the segments. To use the opposite orientation, use a '-' (minus) sign in front of the segment set id.
   CIRCTYP.EQ.1/11/21: The current is imposed through this segment set
   CIRCTYP.EQ.3: The current needed by the circuit equations is measured  through this segment set
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: sidvin
   :type: Optional[int]


   
   Get or set the Segment set ID for input voltage or input current when CIRCTYP.EQ.2/3/12/22 or CIRCTYP.EQ.1/11/21 respectively. It is considered to be oriented as going into the structural mesh, irrespective of the orientation of the segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: sidvout
   :type: Optional[int]


   
   Get or set the Segment set ID for output voltage or output current when CIRCTYP.EQ.2/3/12/22 or CIRCTYP.EQ.1/11/21 repecitively. It is considered to be oriented as going out of the structural mesh, irrespective of the orientation of the segment
















   ..
       !! processed by numpydoc !!

.. py:property:: partid
   :type: Optional[int]


   
   Get or set the Part ID associated to the Circuit. It can be any part ID associated to the circuit.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CIRCUIT'






