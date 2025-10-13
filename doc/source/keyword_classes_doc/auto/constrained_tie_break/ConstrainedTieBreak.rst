





:class:`ConstrainedTieBreak`
============================


.. py:class:: constrained_tie_break.ConstrainedTieBreak(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_TIE-BREAK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedTieBreak

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nsid1`
            - Get or set the Node set ID for nodes on one side of the tied shell edge to shell edge interface; , see *SET_NODE_OPTION.
          * - :py:attr:`~nsid2`
            - Get or set the Node set ID for nodes on the other side of the tied shell edge to shell edge interface, see *SET_NODE.
          * - :py:attr:`~eppf`
            - Get or set the Plastic strain at failure.


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

    from constrained_tie_break import ConstrainedTieBreak

Property detail
---------------

.. py:property:: nsid1
   :type: Optional[int]


   
   Get or set the Node set ID for nodes on one side of the tied shell edge to shell edge interface; , see *SET_NODE_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid2
   :type: Optional[int]


   
   Get or set the Node set ID for nodes on the other side of the tied shell edge to shell edge interface, see *SET_NODE.
















   ..
       !! processed by numpydoc !!

.. py:property:: eppf
   :type: float


   
   Get or set the Plastic strain at failure.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'TIE-BREAK'






