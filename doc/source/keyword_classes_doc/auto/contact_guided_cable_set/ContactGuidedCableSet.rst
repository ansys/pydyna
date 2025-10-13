





:class:`ContactGuidedCableSet`
==============================


.. py:class:: contact_guided_cable_set.ContactGuidedCableSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_GUIDED_CABLE_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactGuidedCableSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Contact interface ID. This must be a unique number.
          * - :py:attr:`~title`
            - Get or set the Interface descriptor. It is suggested that unique descriptions be used.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID that guides the 1D elements
          * - :py:attr:`~psid`
            - Get or set the Part ID or part set ID if SET is included in the keyword line
          * - :py:attr:`~soft`
            - Get or set the Flag for soft constraint option.  Set to 1 for soft constraint.
          * - :py:attr:`~ssfac`
            - Get or set the Stiffness scale factor for penalty stiffness value.  The default value is unity.  This applies to SOFT set to 0 and 1.
          * - :py:attr:`~fric`
            - Get or set the Coefficient of friction.
          * - :py:attr:`~endtol`
            - Get or set the Tolerance, in length units, applied at the ends of the cable elements beyond which contact will pass to the next cable element. The default is 0.002 times the element length


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

    from contact_guided_cable_set import ContactGuidedCableSet

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Contact interface ID. This must be a unique number.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Interface descriptor. It is suggested that unique descriptions be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: Optional[int]


   
   Get or set the Node set ID that guides the 1D elements
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part ID or part set ID if SET is included in the keyword line
















   ..
       !! processed by numpydoc !!

.. py:property:: soft
   :type: int


   
   Get or set the Flag for soft constraint option.  Set to 1 for soft constraint.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssfac
   :type: float


   
   Get or set the Stiffness scale factor for penalty stiffness value.  The default value is unity.  This applies to SOFT set to 0 and 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: Optional[float]


   
   Get or set the Coefficient of friction.
















   ..
       !! processed by numpydoc !!

.. py:property:: endtol
   :type: Optional[float]


   
   Get or set the Tolerance, in length units, applied at the ends of the cable elements beyond which contact will pass to the next cable element. The default is 0.002 times the element length
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'GUIDED_CABLE_SET'






