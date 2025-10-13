





:class:`ConstrainedButtWeld`
============================


.. py:class:: constrained_butt_weld.ConstrainedButtWeld(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_BUTT_WELD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedButtWeld

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid1`
            - Get or set the Node set ID for one side of the butt weld, See *SET_NODE_option.
          * - :py:attr:`~nsid2`
            - Get or set the Node set ID for the other side of the butt weld, See *SET_NODE_option.
          * - :py:attr:`~eppf`
            - Get or set the Plastic Strain at failure
          * - :py:attr:`~sigf`
            - Get or set the Stress at failure for brittle failure
          * - :py:attr:`~beta`
            - Get or set the Failure parameter for brittle failure


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

    from constrained_butt_weld import ConstrainedButtWeld

Property detail
---------------

.. py:property:: nsid1
   :type: Optional[int]


   
   Get or set the Node set ID for one side of the butt weld, See *SET_NODE_option.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid2
   :type: Optional[int]


   
   Get or set the Node set ID for the other side of the butt weld, See *SET_NODE_option.
















   ..
       !! processed by numpydoc !!

.. py:property:: eppf
   :type: float


   
   Get or set the Plastic Strain at failure
















   ..
       !! processed by numpydoc !!

.. py:property:: sigf
   :type: float


   
   Get or set the Stress at failure for brittle failure
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Failure parameter for brittle failure
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'BUTT_WELD'






