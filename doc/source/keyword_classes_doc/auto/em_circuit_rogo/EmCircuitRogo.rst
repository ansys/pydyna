





:class:`EmCircuitRogo`
======================


.. py:class:: em_circuit_rogo.EmCircuitRogo(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CIRCUIT_ROGO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmCircuitRogo

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rogoid`
            - Get or set the Rogowsky coil ID.
          * - :py:attr:`~setid`
            - Get or set the Segment or node set ID.
          * - :py:attr:`~settype`
            - Get or set the Type of set:
          * - :py:attr:`~curtyp`
            - Get or set the Type of current measured:


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

    from em_circuit_rogo import EmCircuitRogo

Property detail
---------------

.. py:property:: rogoid
   :type: Optional[int]


   
   Get or set the Rogowsky coil ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Segment or node set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: settype
   :type: int


   
   Get or set the Type of set:
   EQ.1: Segment set
   EQ.2: Node set (not available yet)
















   ..
       !! processed by numpydoc !!

.. py:property:: curtyp
   :type: int


   
   Get or set the Type of current measured:
   EQ.1: Volume current
   EQ.2: Surface current (not available yet_
   EQ.3: Magnetic field flow (B field times Area)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CIRCUIT_ROGO'






