





:class:`ContactCoupling`
========================


.. py:class:: contact_coupling.ContactCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for coupling
          * - :py:attr:`~sid`
            - Get or set the Set ID for coupling
          * - :py:attr:`~stype`
            - Get or set the EQ.0:part set


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

    from contact_coupling import ContactCoupling

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set ID for coupling
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the EQ.0:part set
   EQ.1:shell element set
   EQ.2:solid element
   EQ.3 thick shell element set
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'COUPLING'






