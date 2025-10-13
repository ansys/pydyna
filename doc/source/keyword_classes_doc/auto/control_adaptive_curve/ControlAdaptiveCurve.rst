





:class:`ControlAdaptiveCurve`
=============================


.. py:class:: control_adaptive_curve.ControlAdaptiveCurve(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_ADAPTIVE_CURVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlAdaptiveCurve

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idset`
            - Get or set the Set id
          * - :py:attr:`~itype`
            - Get or set the EQ.1: IDSET is shell set ID.
          * - :py:attr:`~n`
            - Get or set the Refinement option.
          * - :py:attr:`~smin`
            - Get or set the if the elements is smaller than this value, it will not be refined.
          * - :py:attr:`~itriop`
            - Get or set the Option to refine an enclosed area of a trim curve.


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

    from control_adaptive_curve import ControlAdaptiveCurve

Property detail
---------------

.. py:property:: idset
   :type: Optional[int]


   
   Get or set the Set id
















   ..
       !! processed by numpydoc !!

.. py:property:: itype
   :type: int


   
   Get or set the EQ.1: IDSET is shell set ID.
   EQ.2: IDSET is part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[int]


   
   Get or set the Refinement option.
   1:EQ.1: Refine until there are no adaptive constraints remaining in the element mesh around the curve..
   GT.1: Refine no more than N levels
















   ..
       !! processed by numpydoc !!

.. py:property:: smin
   :type: Optional[float]


   
   Get or set the if the elements is smaller than this value, it will not be refined.
















   ..
       !! processed by numpydoc !!

.. py:property:: itriop
   :type: int


   
   Get or set the Option to refine an enclosed area of a trim curve.
   EQ.0: Refine the elements along the trim curve
   EQ.1: Refine the elements along the trim curve and enclosed by the trim curve..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'ADAPTIVE_CURVE'






