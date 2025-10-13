





:class:`ElementMassPartSet`
===========================


.. py:class:: element_mass_part_set.ElementMassPartSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_MASS_PART_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementMassPartSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part set id, a unique number must be used.
          * - :py:attr:`~addmass`
            - Get or set the Added translational mass to be distributed to nodes of PID.
          * - :py:attr:`~finmass`
            - Get or set the Final translational mass of the part set ID.  The total mass of PSID is computed and subtracted from the final mass of the part or part set to obtain the added translational mass, which must exceed zero.  Set FINMASS to zero if ADDMASS is nonzero.  FINMASS is available in the R3 release of version 971.


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

    from element_mass_part_set import ElementMassPartSet

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set id, a unique number must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: addmass
   :type: float


   
   Get or set the Added translational mass to be distributed to nodes of PID.
















   ..
       !! processed by numpydoc !!

.. py:property:: finmass
   :type: float


   
   Get or set the Final translational mass of the part set ID.  The total mass of PSID is computed and subtracted from the final mass of the part or part set to obtain the added translational mass, which must exceed zero.  Set FINMASS to zero if ADDMASS is nonzero.  FINMASS is available in the R3 release of version 971.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'MASS_PART_SET'






