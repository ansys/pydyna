





:class:`ElementSeatbeltSlipring`
================================


.. py:class:: element_seatbelt_slipring.ElementSeatbeltSlipring(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_SEATBELT_SLIPRING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementSeatbeltSlipring

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sbsrid`
            - Get or set the Slipring ID. A unique number has to be used.
          * - :py:attr:`~sbid1`
            - Get or set the Seat belt element 1 ID
          * - :py:attr:`~sbid2`
            - Get or set the Seat belt element 2 ID
          * - :py:attr:`~fc`
            - Get or set the Coulomb friction coefficient,<0 when |FC| refers to a curve which defines dynamic friction coefficient as a function of time
          * - :py:attr:`~sbrnid`
            - Get or set the Slip ring node, NID
          * - :py:attr:`~ltime`
            - Get or set the Slip ring lockup time. After this time no material is moved from one side of the slip ring to the other. This option is not active during dynamic relaxation.
          * - :py:attr:`~fcs`
            - Get or set the Optional static Coulomb friction coefficient.; <0 when |FCS| refers to a curve which defines static friction coefficient as a function of time
          * - :py:attr:`~onid`
            - Get or set the Optional orientation node ID used to define the skew angle, (see maunal Figure 0-1 and Figure 0-4).  If ONID undefined, the skew angle is assumed to be 0.0.
          * - :py:attr:`~k`
            - Get or set the Optional coefficient for determining the Coulomb friction coefficient related to angle alpha
          * - :py:attr:`~funcid_`
            - Get or set the Function ID to determine friction coefficient
          * - :py:attr:`~direct`
            - Get or set the DIRECT: = 12 if the belt is only allowed to slip along the direction from SBID1 to SBID2
          * - :py:attr:`~dc`
            - Get or set the Optional decay constant to allow a smooth transition between the static and dynamic friction coefficients,
          * - :py:attr:`~lcnffd`
            - Get or set the Optional curve for normal-force-dependent Coulomb dynamic friction
          * - :py:attr:`~lcnffs`
            - Get or set the Optional curve for normal-force-dependent Coulomb static friction


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

    from element_seatbelt_slipring import ElementSeatbeltSlipring

Property detail
---------------

.. py:property:: sbsrid
   :type: int


   
   Get or set the Slipring ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sbid1
   :type: int


   
   Get or set the Seat belt element 1 ID
















   ..
       !! processed by numpydoc !!

.. py:property:: sbid2
   :type: int


   
   Get or set the Seat belt element 2 ID
















   ..
       !! processed by numpydoc !!

.. py:property:: fc
   :type: float


   
   Get or set the Coulomb friction coefficient,<0 when |FC| refers to a curve which defines dynamic friction coefficient as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: sbrnid
   :type: int


   
   Get or set the Slip ring node, NID
















   ..
       !! processed by numpydoc !!

.. py:property:: ltime
   :type: float


   
   Get or set the Slip ring lockup time. After this time no material is moved from one side of the slip ring to the other. This option is not active during dynamic relaxation.
















   ..
       !! processed by numpydoc !!

.. py:property:: fcs
   :type: float


   
   Get or set the Optional static Coulomb friction coefficient.; <0 when |FCS| refers to a curve which defines static friction coefficient as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: onid
   :type: Optional[int]


   
   Get or set the Optional orientation node ID used to define the skew angle, (see maunal Figure 0-1 and Figure 0-4).  If ONID undefined, the skew angle is assumed to be 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Optional coefficient for determining the Coulomb friction coefficient related to angle alpha
















   ..
       !! processed by numpydoc !!

.. py:property:: funcid_
   :type: Optional[int]


   
   Get or set the Function ID to determine friction coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: direct
   :type: Optional[int]


   
   Get or set the DIRECT: = 12 if the belt is only allowed to slip along the direction from SBID1 to SBID2
   =21 if the belt is only allowed to slip along the direction from SBID2 to SBID1
   =0 if the belt can move along both directions.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: Optional[float]


   
   Get or set the Optional decay constant to allow a smooth transition between the static and dynamic friction coefficients,
















   ..
       !! processed by numpydoc !!

.. py:property:: lcnffd
   :type: int


   
   Get or set the Optional curve for normal-force-dependent Coulomb dynamic friction
   coefficient. When defined, the dynamic friction coefficient becomes
   FC+fLCNFFD(Fn), where fLCNFFD(Fn) is the function value of LCNFFD when
   contact force equals Fn,The normal direction is defined as the average of the directions of attached elements SBID1 and SBID2.  The normal force, or contact force, F_n is the summation of T_1  and T_2, the  forces of attached elements,  along the normal direction
















   ..
       !! processed by numpydoc !!

.. py:property:: lcnffs
   :type: int


   
   Get or set the Optional curve for normal-force-dependent Coulomb static friction
   coefficient. When defined, the static friction coefficient becomes
   FCS+fLCNFFS(Fn), where fLCNFFS(Fn) is the function value of LCNFFS when
   contact force equals Fn,
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'SEATBELT_SLIPRING'






