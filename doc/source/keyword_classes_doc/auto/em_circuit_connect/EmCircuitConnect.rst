





:class:`EmCircuitConnect`
=========================


.. py:class:: em_circuit_connect.EmCircuitConnect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CIRCUIT_CONNECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmCircuitConnect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~conid`
            - Get or set the Id of the Circuit Connect
          * - :py:attr:`~contype`
            - Get or set the Type of connection between circuits. For the moment, it is only possible to combine circuits by imposing a linear constraint on the global current (=1).
          * - :py:attr:`~circ1`
            - Get or set the circuit 1
          * - :py:attr:`~circ2`
            - Get or set the circuit 2
          * - :py:attr:`~c1`
            - Get or set the Values of the linear constraints if CONTYPE = 1.
          * - :py:attr:`~c2`
            - Get or set the Values of the linear constraints if CONTYPE = 1.


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

    from em_circuit_connect import EmCircuitConnect

Property detail
---------------

.. py:property:: conid
   :type: Optional[int]


   
   Get or set the Id of the Circuit Connect
















   ..
       !! processed by numpydoc !!

.. py:property:: contype
   :type: Optional[int]


   
   Get or set the Type of connection between circuits. For the moment, it is only possible to combine circuits by imposing a linear constraint on the global current (=1).
















   ..
       !! processed by numpydoc !!

.. py:property:: circ1
   :type: Optional[int]


   
   Get or set the circuit 1
















   ..
       !! processed by numpydoc !!

.. py:property:: circ2
   :type: Optional[int]


   
   Get or set the circuit 2
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Values of the linear constraints if CONTYPE = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Values of the linear constraints if CONTYPE = 1.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CIRCUIT_CONNECT'






