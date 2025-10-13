





:class:`DefineDeToSurfaceCoupling`
==================================


.. py:class:: define_de_to_surface_coupling.DefineDeToSurfaceCoupling(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_TO_SURFACE_COUPLING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeToSurfaceCoupling

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~slave`
            - Get or set the Slave Set ID
          * - :py:attr:`~master`
            - Get or set the Master shell Set ID
          * - :py:attr:`~stype`
            - Get or set the EQ.0: Slave node set
          * - :py:attr:`~mtype`
            - Get or set the EQ.0: Part set
          * - :py:attr:`~frics`
            - Get or set the Friction coefficient
          * - :py:attr:`~fricd`
            - Get or set the Rolling friction coefficient
          * - :py:attr:`~damp`
            - Get or set the Damping coefficient
          * - :py:attr:`~bsort`
            - Get or set the Number of cycle between bucket sort. (Default=100) .LT.0: ABS(BSORT) is the minimum number of cycle between bucket sort.  This value can be increased during runtime by tracking the velocity of potential coupling pair.  This feature only works with MPP currently.
          * - :py:attr:`~lcvx`
            - Get or set the Load curve defines surface velocity in X direction.
          * - :py:attr:`~lcvy`
            - Get or set the Load curve defines surface velocity in Y direction.
          * - :py:attr:`~lcvz`
            - Get or set the Load curve defines surface velocity in Z direction.
          * - :py:attr:`~wearc`
            - Get or set the WEARC is the wear coefficient..
          * - :py:attr:`~w1`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~w2`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~w3`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~w4`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~w5`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~w6`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~w7`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~w8`
            - Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
          * - :py:attr:`~sfp`
            - Get or set the Scale factor on contact stiffness. By default, SFP = 1.0
          * - :py:attr:`~sft`
            - Get or set the Scale factor for surface thickness (scales true thickness). This option
          * - :py:attr:`~cid_rcf`
            - Get or set the Coordinate system ID to output demrcf force resultants in a local system.
          * - :py:attr:`~bt`
            - Get or set the Birth time
          * - :py:attr:`~dt`
            - Get or set the Death time
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

    from define_de_to_surface_coupling import DefineDeToSurfaceCoupling

Property detail
---------------

.. py:property:: slave
   :type: Optional[int]


   
   Get or set the Slave Set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: master
   :type: Optional[int]


   
   Get or set the Master shell Set ID
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the EQ.0: Slave node set
   EQ.1: Slave node
   EQ.2: Slave part set
   EQ.3: Slave part
















   ..
       !! processed by numpydoc !!

.. py:property:: mtype
   :type: int


   
   Get or set the EQ.0: Part set
   EQ.1: Part
















   ..
       !! processed by numpydoc !!

.. py:property:: frics
   :type: Optional[float]


   
   Get or set the Friction coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: fricd
   :type: Optional[float]


   
   Get or set the Rolling friction coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: damp
   :type: Optional[float]


   
   Get or set the Damping coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: bsort
   :type: int


   
   Get or set the Number of cycle between bucket sort. (Default=100) .LT.0: ABS(BSORT) is the minimum number of cycle between bucket sort.  This value can be increased during runtime by tracking the velocity of potential coupling pair.  This feature only works with MPP currently.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvx
   :type: int


   
   Get or set the Load curve defines surface velocity in X direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvy
   :type: int


   
   Get or set the Load curve defines surface velocity in Y direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcvz
   :type: int


   
   Get or set the Load curve defines surface velocity in Z direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: wearc
   :type: float


   
   Get or set the WEARC is the wear coefficient..
















   ..
       !! processed by numpydoc !!

.. py:property:: w1
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w2
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w3
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w4
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w5
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w6
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w7
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: w8
   :type: Optional[float]


   
   Get or set the WEARC = -1, W1 is yield stress of target material, WEARC = -100, user defined wear parameters
















   ..
       !! processed by numpydoc !!

.. py:property:: sfp
   :type: float


   
   Get or set the Scale factor on contact stiffness. By default, SFP = 1.0
















   ..
       !! processed by numpydoc !!

.. py:property:: sft
   :type: float


   
   Get or set the Scale factor for surface thickness (scales true thickness). This option
   applies only to contact with shell elements. True thickness is the      element thickness of the shell elements
















   ..
       !! processed by numpydoc !!

.. py:property:: cid_rcf
   :type: int


   
   Get or set the Coordinate system ID to output demrcf force resultants in a local system.
















   ..
       !! processed by numpydoc !!

.. py:property:: bt
   :type: float


   
   Get or set the Birth time
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: float


   
   Get or set the Death time
















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
   :value: 'DE_TO_SURFACE_COUPLING'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





