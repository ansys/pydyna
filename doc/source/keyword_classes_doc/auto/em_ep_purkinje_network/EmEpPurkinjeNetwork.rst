





:class:`EmEpPurkinjeNetwork`
============================


.. py:class:: em_ep_purkinje_network.EmEpPurkinjeNetwork(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EP_PURKINJE_NETWORK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmEpPurkinjeNetwork

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~purkid`
            - Get or set the Material ID: refers to MID in the *PART card
          * - :py:attr:`~buildnet`
            - Get or set the If EQ.1, creates a new Purkinje network.
          * - :py:attr:`~ssid`
            - Get or set the Segment set on which the Purkinje network is lying
          * - :py:attr:`~mid`
            - Get or set the Material ID defined in the *MAT section
          * - :py:attr:`~pointstx`
            - Get or set the X coordinate of the tree origin
          * - :py:attr:`~pointsty`
            - Get or set the Y coordinate of the tree origin
          * - :py:attr:`~pointstz`
            - Get or set the Z coordinate of the tree origin
          * - :py:attr:`~edgetlen`
            - Get or set the Edge length
          * - :py:attr:`~numgen`
            - Get or set the Number of generations of branches
          * - :py:attr:`~numbrinit`
            - Get or set the Number of branches attaches to the tree origin
          * - :py:attr:`~numsplit`
            - Get or set the Number of children branches at each node of the tree
          * - :py:attr:`~inodestld`
            - Get or set the Initial node id
          * - :py:attr:`~iedgestld`
            - Get or set the Initial edge id


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

    from em_ep_purkinje_network import EmEpPurkinjeNetwork

Property detail
---------------

.. py:property:: purkid
   :type: Optional[int]


   
   Get or set the Material ID: refers to MID in the *PART card
















   ..
       !! processed by numpydoc !!

.. py:property:: buildnet
   :type: Optional[int]


   
   Get or set the If EQ.1, creates a new Purkinje network.
   EQ.0: does not
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Segment set on which the Purkinje network is lying
















   ..
       !! processed by numpydoc !!

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID defined in the *MAT section
















   ..
       !! processed by numpydoc !!

.. py:property:: pointstx
   :type: Optional[float]


   
   Get or set the X coordinate of the tree origin
















   ..
       !! processed by numpydoc !!

.. py:property:: pointsty
   :type: Optional[float]


   
   Get or set the Y coordinate of the tree origin
















   ..
       !! processed by numpydoc !!

.. py:property:: pointstz
   :type: Optional[float]


   
   Get or set the Z coordinate of the tree origin
















   ..
       !! processed by numpydoc !!

.. py:property:: edgetlen
   :type: Optional[float]


   
   Get or set the Edge length
















   ..
       !! processed by numpydoc !!

.. py:property:: numgen
   :type: Optional[int]


   
   Get or set the Number of generations of branches
















   ..
       !! processed by numpydoc !!

.. py:property:: numbrinit
   :type: Optional[int]


   
   Get or set the Number of branches attaches to the tree origin
















   ..
       !! processed by numpydoc !!

.. py:property:: numsplit
   :type: Optional[int]


   
   Get or set the Number of children branches at each node of the tree
















   ..
       !! processed by numpydoc !!

.. py:property:: inodestld
   :type: Optional[int]


   
   Get or set the Initial node id
















   ..
       !! processed by numpydoc !!

.. py:property:: iedgestld
   :type: Optional[int]


   
   Get or set the Initial edge id
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EP_PURKINJE_NETWORK'






