





:class:`DampingFrequencyRangeDeform`
====================================


.. py:class:: damping_frequency_range_deform.DampingFrequencyRangeDeform(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DAMPING_FREQUENCY_RANGE_DEFORM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DampingFrequencyRangeDeform

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cdamp`
            - Get or set the Damping in fraction of critical.  Accurate application of this damping depends on the time step being small compared to the period of interest.
          * - :py:attr:`~flow`
            - Get or set the Lowest frequency in range of interest (cycles per unit time, e.g. Hz if time unit is seconds)
          * - :py:attr:`~fhigh`
            - Get or set the Highest frequency in range of interest (cycles per unit time, e.g. Hz if time unit is seconds)
          * - :py:attr:`~psid`
            - Get or set the Part set ID. The requested damping is applied only to the parts in the set. If PSID = 0, the damping is applied to all parts except those referred to by other *DAMPING_FREQUENCY_RANGE cards.
          * - :py:attr:`~blank`
            - Get or set the Damping in fraction of critical.
          * - :py:attr:`~pidrel`
            - Get or set the Optional part ID of rigid body. Damping is then applied to the motion relative to the rigid body motion.  This input does not apply to the DEFORM option.
          * - :py:attr:`~iflg`
            - Get or set the Method used for internal calculation of damping constants:


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

    from damping_frequency_range_deform import DampingFrequencyRangeDeform

Property detail
---------------

.. py:property:: cdamp
   :type: float


   
   Get or set the Damping in fraction of critical.  Accurate application of this damping depends on the time step being small compared to the period of interest.
















   ..
       !! processed by numpydoc !!

.. py:property:: flow
   :type: float


   
   Get or set the Lowest frequency in range of interest (cycles per unit time, e.g. Hz if time unit is seconds)
















   ..
       !! processed by numpydoc !!

.. py:property:: fhigh
   :type: float


   
   Get or set the Highest frequency in range of interest (cycles per unit time, e.g. Hz if time unit is seconds)
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID. The requested damping is applied only to the parts in the set. If PSID = 0, the damping is applied to all parts except those referred to by other *DAMPING_FREQUENCY_RANGE cards.
















   ..
       !! processed by numpydoc !!

.. py:property:: blank
   :type: int


   
   Get or set the Damping in fraction of critical.
















   ..
       !! processed by numpydoc !!

.. py:property:: pidrel
   :type: int


   
   Get or set the Optional part ID of rigid body. Damping is then applied to the motion relative to the rigid body motion.  This input does not apply to the DEFORM option.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflg
   :type: int


   
   Get or set the Method used for internal calculation of damping constants:
   EQ.0:   iterative(more accurate)
   EQ.1 : approximate(same as R9 and previous versions)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DAMPING'


.. py:attribute:: subkeyword
   :value: 'FREQUENCY_RANGE_DEFORM'






