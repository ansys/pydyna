





:class:`ConstrainedRivet`
=========================


.. py:class:: constrained_rivet.ConstrainedRivet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_RIVET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedRivet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the Node ID for node 1.
          * - :py:attr:`~n2`
            - Get or set the Node ID for node 2.
          * - :py:attr:`~tf`
            - Get or set the Failure time for nodal constraint set.
          * - :py:attr:`~id`
            - Get or set the ID keyword option


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

    from constrained_rivet import ConstrainedRivet

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node ID for node 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Node ID for node 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: tf
   :type: float


   
   Get or set the Failure time for nodal constraint set.
















   ..
       !! processed by numpydoc !!

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID keyword option
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'RIVET'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





