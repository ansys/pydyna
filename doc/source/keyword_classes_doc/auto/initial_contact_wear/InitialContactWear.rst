





:class:`InitialContactWear`
===========================


.. py:class:: initial_contact_wear.InitialContactWear(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_CONTACT_WEAR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialContactWear

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the contact Interface ID.
          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~wdepth`
            - Get or set the wear depth, in units of length.
          * - :py:attr:`~nx`
            - Get or set the Direction vector for wear, internally normalized.
          * - :py:attr:`~ny`
            - Get or set the Direction vector for wear, internally normalized.
          * - :py:attr:`~nz`
            - Get or set the Direction vector for wear, internally normalized.
          * - :py:attr:`~iseq`
            - Get or set the Simulation sequence number for the entire process.
          * - :py:attr:`~ncyc`
            - Get or set the Number of process cycles this particular simulation corresponds to, a negative number means that LS-DYNA will not apply this card.


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

    from initial_contact_wear import InitialContactWear

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the contact Interface ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: wdepth
   :type: Optional[float]


   
   Get or set the wear depth, in units of length.
















   ..
       !! processed by numpydoc !!

.. py:property:: nx
   :type: Optional[float]


   
   Get or set the Direction vector for wear, internally normalized.
















   ..
       !! processed by numpydoc !!

.. py:property:: ny
   :type: Optional[float]


   
   Get or set the Direction vector for wear, internally normalized.
















   ..
       !! processed by numpydoc !!

.. py:property:: nz
   :type: Optional[float]


   
   Get or set the Direction vector for wear, internally normalized.
















   ..
       !! processed by numpydoc !!

.. py:property:: iseq
   :type: Optional[int]


   
   Get or set the Simulation sequence number for the entire process.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncyc
   :type: Optional[int]


   
   Get or set the Number of process cycles this particular simulation corresponds to, a negative number means that LS-DYNA will not apply this card.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'CONTACT_WEAR'






