





:class:`ContactInterior`
========================


.. py:class:: contact_interior.ContactInterior(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_INTERIOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactInterior

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part set ID including all parts for which interior contact is desired.


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

    from contact_interior import ContactInterior

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID including all parts for which interior contact is desired.
   Three attributes should be defined for the part set:
   Attribute 1: PSF, penalty scale factor (default=1.00).
   Attribute 2: Activation factor, Fa (default=0.10).When the crushing of the element reaches Fa times the initial thickness the contact algorithm begins to act.
   Attribute 3: ED, Optional modulus for interior contact stiffness.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'INTERIOR'






