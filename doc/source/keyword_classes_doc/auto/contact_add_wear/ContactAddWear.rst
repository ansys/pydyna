





:class:`ContactAddWear`
=======================


.. py:class:: contact_add_wear.ContactAddWear(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_ADD_WEAR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ContactAddWear

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Contact interface ID, see *CONTACT_...
          * - :py:attr:`~wtype`
            - Get or set the Wear law:
          * - :py:attr:`~p1`
            - Get or set the First wear parameter.
          * - :py:attr:`~p2`
            - Get or set the Second wear parameter.
          * - :py:attr:`~p3`
            - Get or set the Third wear parameter.
          * - :py:attr:`~p4`
            - Get or set the Not used
          * - :py:attr:`~p5`
            - Get or set the Not used
          * - :py:attr:`~p6`
            - Get or set the Not used
          * - :py:attr:`~w1`
            - Get or set the User defined wear parameter
          * - :py:attr:`~w2`
            - Get or set the User defined wear parameter
          * - :py:attr:`~w3`
            - Get or set the User defined wear parameter
          * - :py:attr:`~w4`
            - Get or set the User defined wear parameter
          * - :py:attr:`~w5`
            - Get or set the User defined wear parameter
          * - :py:attr:`~w6`
            - Get or set the User defined wear parameter
          * - :py:attr:`~w7`
            - Get or set the User defined wear parameter
          * - :py:attr:`~w8`
            - Get or set the User defined wear parameter


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

    from contact_add_wear import ContactAddWear

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Contact interface ID, see *CONTACT_...
   LT.0:   Perturb contact surface according to wear values (see Remark 5).
   GT.0:   Calculate wear properties for post - processing only.
















   ..
       !! processed by numpydoc !!

.. py:property:: wtype
   :type: int


   
   Get or set the Wear law:
   LT.0: user defined wear law, value denotes type used in subroutine
   EQ.0 Archard's wear law. d(wear_depth)/dt = K*contact_pressure*sliding_velocity/H
















   ..
       !! processed by numpydoc !!

.. py:property:: p1
   :type: Optional[float]


   
   Get or set the First wear parameter.
   WTYPE.EQ.0: Dimensionless parameter, K. If negative the absolute value denotes table ID with K = K(p, d) as a function of contact pressure p >= 0 and relative sliding velocity d >= 0.
   WTYPE.LT.0: Number of user wear parameters for this interface.
















   ..
       !! processed by numpydoc !!

.. py:property:: p2
   :type: Optional[float]


   
   Get or set the Second wear parameter.
   WTYPE.EQ.0: SURFA surface hardness parameter, Hs. If negative the absolute value denotes curve ID with Hs = Hs(Ts) as function of SURFA node temperature Ts.
   WTYPE.LT.0: Number of user wear history variables per contact node.
















   ..
       !! processed by numpydoc !!

.. py:property:: p3
   :type: Optional[float]


   
   Get or set the Third wear parameter.
   WTYPE.EQ.0: SURFB surface hardness parameter, Hm. If negative the absolute value denotes curve ID with Hm = Hm(Tm) as function of SURFB node temperature Tm.
   WTYPE.LT.0: Not used
















   ..
       !! processed by numpydoc !!

.. py:property:: p4
   :type: Optional[float]


   
   Get or set the Not used
















   ..
       !! processed by numpydoc !!

.. py:property:: p5
   :type: Optional[float]


   
   Get or set the Not used
















   ..
       !! processed by numpydoc !!

.. py:property:: p6
   :type: Optional[float]


   
   Get or set the Not used
















   ..
       !! processed by numpydoc !!

.. py:property:: w1
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: w2
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: w3
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: w4
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: w5
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: w6
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: w7
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: w8
   :type: Optional[float]


   
   Get or set the User defined wear parameter
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: 'ADD_WEAR'






