





:class:`DualceseEosVanDerWaalsGeneralized`
==========================================


.. py:class:: dualcese_eos_van_der_waals_generalized.DualceseEosVanDerWaalsGeneralized(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_EOS_VAN_DER_WAALS_GENERALIZED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseEosVanDerWaalsGeneralized

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eosid`
            - Get or set the Equation of state ID
          * - :py:attr:`~a`
            - Get or set the van der Waals gas constant for molecular cohesive forces
          * - :py:attr:`~b`
            - Get or set the van der Waals gas constant for the finite size of molecules
          * - :py:attr:`~ga`
            - Get or set the Ratio of specific heats, Must be>1.0.
          * - :py:attr:`~bt`
            - Get or set the Reference pressure, Must be>+0.0


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

    from dualcese_eos_van_der_waals_generalized import DualceseEosVanDerWaalsGeneralized

Property detail
---------------

.. py:property:: eosid
   :type: Optional[int]


   
   Get or set the Equation of state ID
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the van der Waals gas constant for molecular cohesive forces
















   ..
       !! processed by numpydoc !!

.. py:property:: b
   :type: Optional[float]


   
   Get or set the van der Waals gas constant for the finite size of molecules
















   ..
       !! processed by numpydoc !!

.. py:property:: ga
   :type: Optional[float]


   
   Get or set the Ratio of specific heats, Must be>1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: Optional[float]


   
   Get or set the Reference pressure, Must be>+0.0
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'EOS_VAN_DER_WAALS_GENERALIZED'






