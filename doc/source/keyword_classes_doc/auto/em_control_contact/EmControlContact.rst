





:class:`EmControlContact`
=========================


.. py:class:: em_control_contact.EmControlContact(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTROL_CONTACT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmControlContact

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~emct`
            - Get or set the EM contact activation flag:
          * - :py:attr:`~cconly`
            - Get or set the Determines on which parts of the model the EM contact should be activated.
          * - :py:attr:`~ctype`
            - Get or set the Contact type :
          * - :py:attr:`~cotype`
            - Get or set the Type of EM contact. If *EM_CONTACT is not defined, the solver will look for global contact options in *EM_CONTROL_CONTACT.
          * - :py:attr:`~eps1`
            - Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
          * - :py:attr:`~eps2`
            - Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
          * - :py:attr:`~eps3`
            - Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
          * - :py:attr:`~d0`
            - Get or set the Global contact condition 3 value when COTYPE = 1


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

    from em_control_contact import EmControlContact

Property detail
---------------

.. py:property:: emct
   :type: int


   
   Get or set the EM contact activation flag:
   EQ.0: no contact detection
   EQ.1: contact detection
















   ..
       !! processed by numpydoc !!

.. py:property:: cconly
   :type: int


   
   Get or set the Determines on which parts of the model the EM contact should be activated.
   EQ.0: Contact detection between all active parts associated with a conducting material.
   EQ.1: Only look for EM contact between parts associated through the EM_CONTACT card.In some cases this option can reduce the calculation time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Contact type :
   EQ. - 1:        Node to node contact based on constraints on the scalar potential.See Remark 1.
   EQ.0 : Node to node penalty based contact on the scalar potential.
   EQ.1 : Discrete mortar penalty contact on the scalar potential.
   EQ.2 : Continuous mortar penalty contact on the scalar potential and the vector potential(when active).
















   ..
       !! processed by numpydoc !!

.. py:property:: cotype
   :type: int


   
   Get or set the Type of EM contact. If *EM_CONTACT is not defined, the solver will look for global contact options in *EM_CONTROL_CONTACT.
   EQ.0: Contact type 0.
   EQ.1: Contact type 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps1
   :type: float


   
   Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps2
   :type: float


   
   Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps3
   :type: float


   
   Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
















   ..
       !! processed by numpydoc !!

.. py:property:: d0
   :type: Optional[float]


   
   Get or set the Global contact condition 3 value when COTYPE = 1
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTROL_CONTACT'






