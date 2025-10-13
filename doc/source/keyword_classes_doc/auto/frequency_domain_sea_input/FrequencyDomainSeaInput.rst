





:class:`FrequencyDomainSeaInput`
================================


.. py:class:: frequency_domain_sea_input.FrequencyDomainSeaInput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_SEA_INPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainSeaInput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~subid`
            - Get or set the Subsystem ID.
          * - :py:attr:`~subtyp`
            - Get or set the Subsystem type
          * - :py:attr:`~loadtyp`
            - Get or set the Input power type:
          * - :py:attr:`~bwave`
            - Get or set the Input power value for bending wave.
          * - :py:attr:`~lwave`
            - Get or set the Input power value for longitudinal wave.
          * - :py:attr:`~swave`
            - Get or set the Input power value for shear wave.
          * - :py:attr:`~twave`
            - Get or set the Input power value for shear wave.


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

    from frequency_domain_sea_input import FrequencyDomainSeaInput

Property detail
---------------

.. py:property:: subid
   :type: Optional[int]


   
   Get or set the Subsystem ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: subtyp
   :type: int


   
   Get or set the Subsystem type
   EQ.1: plate
   EQ.2: cavity
   EQ.3: beam.
















   ..
       !! processed by numpydoc !!

.. py:property:: loadtyp
   :type: int


   
   Get or set the Input power type:
   EQ.0: power
   EQ.1: force
   EQ.2: velocity
   EQ.3: pressure.
   EQ.4:   bending wave power for plate
   EQ.5:   shear wave power for plate
















   ..
       !! processed by numpydoc !!

.. py:property:: bwave
   :type: Optional[float]


   
   Get or set the Input power value for bending wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: lwave
   :type: Optional[float]


   
   Get or set the Input power value for longitudinal wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: swave
   :type: Optional[float]


   
   Get or set the Input power value for shear wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: twave
   :type: Optional[float]


   
   Get or set the Input power value for shear wave.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_SEA_INPUT'






