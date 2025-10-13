





:class:`FrequencyDomainSeaConnection`
=====================================


.. py:class:: frequency_domain_sea_connection.FrequencyDomainSeaConnection(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_SEA_CONNECTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainSeaConnection

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~conid`
            - Get or set the Connection ID.
          * - :py:attr:`~ctype`
            - Get or set the Type of connection
          * - :py:attr:`~nsub`
            - Get or set the Number of subsystems in this connection.
          * - :py:attr:`~ibeam`
            - Get or set the Flag for plate connected to plate
          * - :py:attr:`~sub1`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~sub2`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~sub3`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~sub4`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~sub5`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~sub6`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~sub7`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~sub8`
            - Get or set the ID of the ith subsystem.
          * - :py:attr:`~ang1`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~ang2`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~ang3`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~ang4`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~ang5`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~ang6`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~ang7`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~ang8`
            - Get or set the Connection angle of the plate i.
          * - :py:attr:`~length`
            - Get or set the Length of the edge in connection.
          * - :py:attr:`~absorb`
            - Get or set the Absorption coefficient.
          * - :py:attr:`~thick`
            - Get or set the Thickness of the plate.


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

    from frequency_domain_sea_connection import FrequencyDomainSeaConnection

Property detail
---------------

.. py:property:: conid
   :type: Optional[int]


   
   Get or set the Connection ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Type of connection
   EQ.1: plate-plate
   EQ.2: plate-cavity
   EQ.3: plate-cavity-cavity
   EQ.4: plate-beam
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: nsub
   :type: Optional[int]


   
   Get or set the Number of subsystems in this connection.
















   ..
       !! processed by numpydoc !!

.. py:property:: ibeam
   :type: int


   
   Get or set the Flag for plate connected to plate
   EQ.0:   plate - plate connection.
   EQ.1 : plate - plate - beam connection.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub1
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub2
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub3
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub4
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub5
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub6
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub7
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: sub8
   :type: Optional[int]


   
   Get or set the ID of the ith subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang1
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang2
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang3
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang4
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang5
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang6
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang7
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang8
   :type: Optional[float]


   
   Get or set the Connection angle of the plate i.
















   ..
       !! processed by numpydoc !!

.. py:property:: length
   :type: float


   
   Get or set the Length of the edge in connection.
















   ..
       !! processed by numpydoc !!

.. py:property:: absorb
   :type: float


   
   Get or set the Absorption coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: float


   
   Get or set the Thickness of the plate.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_SEA_CONNECTION'






