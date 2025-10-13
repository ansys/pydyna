





:class:`DualceseReactionRateIgReduced`
======================================


.. py:class:: dualcese_reaction_rate_ig_reduced.DualceseReactionRateIgReduced(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DUALCESE_REACTION_RATE_IG_REDUCED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DualceseReactionRateIgReduced

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~react_id`
            - Get or set the ID of reaction rate law
          * - :py:attr:`~grow1`
            - Get or set the Reaction growth term parameter
          * - :py:attr:`~cc`
            - Get or set the Reaction growth term parameter
          * - :py:attr:`~dd`
            - Get or set the Reaction growth term parameter
          * - :py:attr:`~yy`
            - Get or set the Reaction growth term parameter
          * - :py:attr:`~ph10`
            - Get or set the Additional parameter to account for the non-zero amount of reaction when the mass fraction of the products, is zero


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

    from dualcese_reaction_rate_ig_reduced import DualceseReactionRateIgReduced

Property detail
---------------

.. py:property:: react_id
   :type: Optional[int]


   
   Get or set the ID of reaction rate law
















   ..
       !! processed by numpydoc !!

.. py:property:: grow1
   :type: Optional[float]


   
   Get or set the Reaction growth term parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: cc
   :type: Optional[float]


   
   Get or set the Reaction growth term parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: dd
   :type: Optional[float]


   
   Get or set the Reaction growth term parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: yy
   :type: Optional[float]


   
   Get or set the Reaction growth term parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: ph10
   :type: Optional[float]


   
   Get or set the Additional parameter to account for the non-zero amount of reaction when the mass fraction of the products, is zero
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DUALCESE'


.. py:attribute:: subkeyword
   :value: 'REACTION_RATE_IG_REDUCED'






