





:class:`IcfdBoundaryWindkessel`
===============================


.. py:class:: icfd_boundary_windkessel.IcfdBoundaryWindkessel(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_WINDKESSEL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryWindkessel

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID for a fluid surface.
          * - :py:attr:`~wtype`
            - Get or set the Circuit type (See Remarks) :
          * - :py:attr:`~r1`
            - Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
          * - :py:attr:`~c1`
            - Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
          * - :py:attr:`~r2`
            - Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
          * - :py:attr:`~l1`
            - Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.


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

    from icfd_boundary_windkessel import IcfdBoundaryWindkessel

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID for a fluid surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: wtype
   :type: Optional[int]


   
   Get or set the Circuit type (See Remarks) :
   EQ.1:   Windkessel circuit
   EQ.2:   Windkessel circuit with inverted flux
   EQ.3:   CV type circuit
   EQ.4:   CV type circuit with inverted flux.
















   ..
       !! processed by numpydoc !!

.. py:property:: r1
   :type: float


   
   Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: float


   
   Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
















   ..
       !! processed by numpydoc !!

.. py:property:: r2
   :type: float


   
   Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
















   ..
       !! processed by numpydoc !!

.. py:property:: l1
   :type: float


   
   Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_WINDKESSEL'






