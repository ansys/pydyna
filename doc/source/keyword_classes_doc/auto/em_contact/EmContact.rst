





:class:`EmContact`
==================


.. py:class:: em_contact.EmContact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmContact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~contid`
            - Get or set the Electromagnetic contact ID.
          * - :py:attr:`~conttype`
            - Get or set the Type of EM contact.
          * - :py:attr:`~psidm`
            - Get or set the Set of master parts ID.
          * - :py:attr:`~psids`
            - Get or set the Set of slave parts ID.
          * - :py:attr:`~eps1`
            - Get or set the Contact Coefficients for contact detection conditions.
          * - :py:attr:`~eps2`
            - Get or set the Contact Coefficients for contact detection conditions.
          * - :py:attr:`~eps3`
            - Get or set the Contact Coefficients for contact detection conditions.
          * - :py:attr:`~d0`
            - Get or set the Contact condition 3 when COTYPE = 1.


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

    from em_contact import EmContact

Property detail
---------------

.. py:property:: contid
   :type: Optional[int]


   
   Get or set the Electromagnetic contact ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: conttype
   :type: int


   
   Get or set the Type of EM contact.
   EQ.1: Face to face.
















   ..
       !! processed by numpydoc !!

.. py:property:: psidm
   :type: Optional[int]


   
   Get or set the Set of master parts ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: psids
   :type: Optional[int]


   
   Get or set the Set of slave parts ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: float


   
   Get or set the Contact Coefficients for contact detection conditions.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2
   :type: float


   
   Get or set the Contact Coefficients for contact detection conditions.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3
   :type: float


   
   Get or set the Contact Coefficients for contact detection conditions.
















   ..
       !! processed by numpydoc !!

.. py:property:: d0
   :type: Optional[float]


   
   Get or set the Contact condition 3 when COTYPE = 1.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTACT'






