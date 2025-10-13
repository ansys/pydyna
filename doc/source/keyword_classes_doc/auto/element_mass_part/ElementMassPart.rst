





:class:`ElementMassPart`
========================


.. py:class:: element_mass_part.ElementMassPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_MASS_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementMassPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part id, a unique number must be used.
          * - :py:attr:`~addmass`
            - Get or set the Added translational mass to be distributed to nodes of PID.
          * - :py:attr:`~finmass`
            - Get or set the Final translational mass of the part ID or part set ID.  The total mass of the PID or SID is computed and subtracted from the final mass of the part or part set to obtain the added translational mass, which must exceed zero.  Set FINMASS to zero if ADDMASS is nonzero.  FINMASS is available in the R3 release of version 971.
          * - :py:attr:`~lcid`
            - Get or set the load curve id


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

    from element_mass_part import ElementMassPart

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part id, a unique number must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: addmass
   :type: float


   
   Get or set the Added translational mass to be distributed to nodes of PID.
















   ..
       !! processed by numpydoc !!

.. py:property:: finmass
   :type: float


   
   Get or set the Final translational mass of the part ID or part set ID.  The total mass of the PID or SID is computed and subtracted from the final mass of the part or part set to obtain the added translational mass, which must exceed zero.  Set FINMASS to zero if ADDMASS is nonzero.  FINMASS is available in the R3 release of version 971.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the load curve id
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'MASS_PART'






