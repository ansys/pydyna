





:class:`ConstrainedMultipleGlobal`
==================================


.. py:class:: constrained_multiple_global.ConstrainedMultipleGlobal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_MULTIPLE_GLOBAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedMultipleGlobal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Constraint set identification. All constraint sets should have a unique set ID.
          * - :py:attr:`~nmp`
            - Get or set the Number of nodes to be constrained mutually.
          * - :py:attr:`~nid`
            - Get or set the Nodal ID.
          * - :py:attr:`~dir`
            - Get or set the Direction in three-dimensional space to be constrained
          * - :py:attr:`~coef`
            - Get or set the Coefficient ¦Ánid in constraint equation.


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

    from constrained_multiple_global import ConstrainedMultipleGlobal

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Constraint set identification. All constraint sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nmp
   :type: Optional[int]


   
   Get or set the Number of nodes to be constrained mutually.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Nodal ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction in three-dimensional space to be constrained
   EQ.1: x direction
   EQ.2: y direction
   EQ.3: z direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: coef
   :type: Optional[float]


   
   Get or set the Coefficient ¦Ánid in constraint equation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'MULTIPLE_GLOBAL'






