





:class:`BoundaryCoupled`
========================


.. py:class:: boundary_coupled.BoundaryCoupled(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_COUPLED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryCoupled

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for this coupled boundary.
          * - :py:attr:`~title`
            - Get or set the Descriptive name for this boundary.
          * - :py:attr:`~set`
            - Get or set the Node set ID.
          * - :py:attr:`~type`
            - Get or set the Coupling type:
          * - :py:attr:`~prog`
            - Get or set the Program to couple to EQ.1: MPP-DYNA.


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

    from boundary_coupled import BoundaryCoupled

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for this coupled boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Descriptive name for this boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: set
   :type: Optional[int]


   
   Get or set the Node set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: type
   :type: int


   
   Get or set the Coupling type:
   EQ.1: node set with force feedback
   EQ.2: node set for multiscale spotwelds.
















   ..
       !! processed by numpydoc !!

.. py:property:: prog
   :type: Optional[int]


   
   Get or set the Program to couple to EQ.1: MPP-DYNA.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'COUPLED'






