





:class:`EmIsopotentialConnect`
==============================


.. py:class:: em_isopotential_connect.EmIsopotentialConnect(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_ISOPOTENTIAL_CONNECT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmIsopotentialConnect

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~conid`
            - Get or set the Connection ID.
          * - :py:attr:`~contype`
            - Get or set the Connection type:
          * - :py:attr:`~isoid1`
            - Get or set the ID of the first isopotential to be connected.
          * - :py:attr:`~isoid2`
            - Get or set the ID of the second isopotential to be connected.
          * - :py:attr:`~val`
            - Get or set the Value of the resistance,voltage,or current depending on CONTYPE.Ignored if LCID defined.
          * - :py:attr:`~lcid_rdlid`
            - Get or set the Load curve ID defining the value of the resistance,voltage,or current function of time and depending on CONTYPE.
          * - :py:attr:`~psid`
            - Get or set the Part Set ID where the joule heating corresponding to the resistance r0 in *EM_RANDLES_MESHLESS is added, averaged over the part set.
          * - :py:attr:`~l`
            - Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
          * - :py:attr:`~c`
            - Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
          * - :py:attr:`~v0`
            - Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.


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

    from em_isopotential_connect import EmIsopotentialConnect

Property detail
---------------

.. py:property:: conid
   :type: Optional[int]


   
   Get or set the Connection ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: contype
   :type: int


   
   Get or set the Connection type:
   EQ.1:Short Circuit.
   EQ.2:Resistance.
   EQ.3:Voltage Source.
   EQ.4:Current Source.
   EQ.5:Meshless Randles circuit (used to represent a cell by one lumped Randles circuit)
   EQ.6:R, L, C circuit
















   ..
       !! processed by numpydoc !!

.. py:property:: isoid1
   :type: Optional[int]


   
   Get or set the ID of the first isopotential to be connected.
















   ..
       !! processed by numpydoc !!

.. py:property:: isoid2
   :type: Optional[int]


   
   Get or set the ID of the second isopotential to be connected.
















   ..
       !! processed by numpydoc !!

.. py:property:: val
   :type: Optional[float]


   
   Get or set the Value of the resistance,voltage,or current depending on CONTYPE.Ignored if LCID defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid_rdlid
   :type: Optional[int]


   
   Get or set the Load curve ID defining the value of the resistance,voltage,or current function of time and depending on CONTYPE.
   If not defined,VAL will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part Set ID where the joule heating corresponding to the resistance r0 in *EM_RANDLES_MESHLESS is added, averaged over the part set.
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
















   ..
       !! processed by numpydoc !!

.. py:property:: v0
   :type: Optional[float]


   
   Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'ISOPOTENTIAL_CONNECT'






