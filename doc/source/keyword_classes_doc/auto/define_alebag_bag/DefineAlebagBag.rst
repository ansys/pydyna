





:class:`DefineAlebagBag`
========================


.. py:class:: define_alebag_bag.DefineAlebagBag(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_ALEBAG_BAG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineAlebagBag

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bagid`
            - Get or set the Bag mesh definition ID, referred in *AIRBAG_ADVANCED_ALE.
          * - :py:attr:`~sid`
            - Get or set the Set of Lagrange shell elements to interact with inflator gas
          * - :py:attr:`~sidtype`
            - Get or set the Type of SID
          * - :py:attr:`~cvbag`
            - Get or set the EQ:'NO' or '0' when SID is used only for coupling, and not considered part of a bag.  Therefore it will not be considered part of control volume bag.  The inner bag of a bag-in-bag is an example of application.
          * - :py:attr:`~iblock`
            - Get or set the flag for hole venting blockage effect: 0, no; 1, yes
          * - :py:attr:`~vcof`
            - Get or set the Venting coefficient
          * - :py:attr:`~vset`
            - Get or set the Set representing venting holes
          * - :py:attr:`~vtype`
            - Get or set the Type of VSET :
          * - :py:attr:`~nquad`
            - Get or set the Quadratue rule for coupling slaves to solids (CTYPE 2 only).
          * - :py:attr:`~ctype`
            - Get or set the Coupling type
          * - :py:attr:`~pfac`
            - Get or set the Penalty factor (CTYPE 4 and 5 only).
          * - :py:attr:`~fric`
            - Get or set the Coefficient of friction (DIREC 2 only).
          * - :py:attr:`~frcmin`
            - Get or set the Minimum volume fraction to activate coupling (MCOUP=1)
          * - :py:attr:`~normtyp`
            - Get or set the Penality spring direction(DIREC 1 and 2 ):
          * - :py:attr:`~ileak`
            - Get or set the Leakage control:
          * - :py:attr:`~pleak`
            - Get or set the Leakage control penalty factor
          * - :py:attr:`~norm`
            - Get or set the Shell and segment normal orientation:
          * - :py:attr:`~start`
            - Get or set the Start time for coupling (default=0.0).
          * - :py:attr:`~end`
            - Get or set the End time for coupling (default=1.0E+10).
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from define_alebag_bag import DefineAlebagBag

Property detail
---------------

.. py:property:: bagid
   :type: Optional[int]


   
   Get or set the Bag mesh definition ID, referred in *AIRBAG_ADVANCED_ALE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set of Lagrange shell elements to interact with inflator gas
















   ..
       !! processed by numpydoc !!

.. py:property:: sidtype
   :type: int


   
   Get or set the Type of SID
   EQ:'PSET' or '0' for set of parts
   EQ:'PART' or '1' for part
















   ..
       !! processed by numpydoc !!

.. py:property:: cvbag
   :type: int


   
   Get or set the EQ:'NO' or '0' when SID is used only for coupling, and not considered part of a bag.  Therefore it will not be considered part of control volume bag.  The inner bag of a bag-in-bag is an example of application.
   EQ:'YES' or '1' when SID is part of a control volume bag and will couple with gas
















   ..
       !! processed by numpydoc !!

.. py:property:: iblock
   :type: int


   
   Get or set the flag for hole venting blockage effect: 0, no; 1, yes
















   ..
       !! processed by numpydoc !!

.. py:property:: vcof
   :type: Optional[float]


   
   Get or set the Venting coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: vset
   :type: Optional[int]


   
   Get or set the Set representing venting holes
















   ..
       !! processed by numpydoc !!

.. py:property:: vtype
   :type: int


   
   Get or set the Type of VSET :
   EQ:'PSET' or '0' for set of parts
   EQ:'PART' or '1' for part
   EQ:'SEGSET' or '2' for segment set
















   ..
       !! processed by numpydoc !!

.. py:property:: nquad
   :type: int


   
   Get or set the Quadratue rule for coupling slaves to solids (CTYPE 2 only).
   EQ.0: at nodes only,
   EQ.n: use a rectangular grid of n*n points,
   EQ.-n: at nodes and a rectangular grid of n*n points.
















   ..
       !! processed by numpydoc !!

.. py:property:: ctype
   :type: int


   
   Get or set the Coupling type
   EQ.1: constrained acceleration,
   EQ.2: constrained acceleration and velocity (default),
   EQ.3: constrained acceleration and velocity, normal direction only,
   EQ.4: penalty coupling (Shell and solid Elements),
   EQ.5: penalty coupling allowing erosion in the lagrangian entities (Solid Elements).
   EQ.6: Penalty coupling designed for airbag modeling(testing).DIREC is automatically reset to DIREC=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfac
   :type: float


   
   Get or set the Penalty factor (CTYPE 4 and 5 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Coefficient of friction (DIREC 2 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: frcmin
   :type: float


   
   Get or set the Minimum volume fraction to activate coupling (MCOUP=1)
















   ..
       !! processed by numpydoc !!

.. py:property:: normtyp
   :type: int


   
   Get or set the Penality spring direction(DIREC 1 and 2 ):
   EQ.0: interpolated from node normals(default),
   EQ.1: segment normal.
















   ..
       !! processed by numpydoc !!

.. py:property:: ileak
   :type: int


   
   Get or set the Leakage control:
   EQ.0: none(default),
   EQ.1: weak,
   EQ.2: strong.
















   ..
       !! processed by numpydoc !!

.. py:property:: pleak
   :type: float


   
   Get or set the Leakage control penalty factor
















   ..
       !! processed by numpydoc !!

.. py:property:: norm
   :type: int


   
   Get or set the Shell and segment normal orientation:
   EQ.0: right hand rule (default)
   EQ.1: left hand rule.
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Start time for coupling (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: end
   :type: float


   
   Get or set the End time for coupling (default=1.0E+10).
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'ALEBAG_BAG'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





