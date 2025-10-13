





:class:`EmContactResistance`
============================


.. py:class:: em_contact_resistance.EmContactResistance(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTACT_RESISTANCE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmContactResistance

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~crid`
            - Get or set the Resistive contact ID.
          * - :py:attr:`~contid`
            - Get or set the EM contact ID defined in *EM_CONTACT.
          * - :py:attr:`~ctype`
            - Get or set the Contact Resistance type :
          * - :py:attr:`~jhrtype`
            - Get or set the Indicates how the Joule heating calculated by the contact resistance shall be taken into account:
          * - :py:attr:`~dfid`
            - Get or set the Load Function ID defining the contact resistance.


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

    from em_contact_resistance import EmContactResistance

Property detail
---------------

.. py:property:: crid
   :type: Optional[int]


   
   Get or set the Resistive contact ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: contid
   :type: Optional[int]


   
   Get or set the EM contact ID defined in *EM_CONTACT.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Contact Resistance type :
   EQ.1: Contact resistance defined by user defined load curve.
   EQ.2: Classic Holm's formula for contact resistances (See Remark 1).
   EQ.3: Modified contact resistance for cases with plastic deformation in the contact area (See Remarks 2 and 3).
   EQ.4: Modified contact resistance for cases with elasticdeformation in the contact area (See Remarks 2 and 3).
   EQ.5: Basic contact resistance definition (See Remark 4).
















   ..
       !! processed by numpydoc !!

.. py:property:: jhrtype
   :type: int


   
   Get or set the Indicates how the Joule heating calculated by the contact resistance shall be taken into account:
   EQ.0: No addition: The Joule heating calculated by the contact resistance is not taken into account.
   EQ.1: The Joule heating coming from the contact resistance is divided and distributed evenly among all elements neighboring the contact surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: dfid
   :type: Optional[int]


   
   Get or set the Load Function ID defining the contact resistance.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTACT_RESISTANCE'






