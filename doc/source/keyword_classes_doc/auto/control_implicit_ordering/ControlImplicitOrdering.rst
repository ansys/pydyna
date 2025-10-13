





:class:`ControlImplicitOrdering`
================================


.. py:class:: control_implicit_ordering.ControlImplicitOrdering(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_ORDERING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitOrdering

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~order`
            - Get or set the Ordering option (see Remark 1):
          * - :py:attr:`~nmetis`
            - Get or set the Number of times to use Metis on each compute node for MPP.


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

    from control_implicit_ordering import ControlImplicitOrdering

Property detail
---------------

.. py:property:: order
   :type: int


   
   Get or set the Ordering option (see Remark 1):
   EQ.0:   method set automatically by LS - DYNA
   EQ.1 : MMD, Multiple Minimum Degree.
   EQ.2 : Metis(see Remark 2)
   EQ.4 : LS - GPart
















   ..
       !! processed by numpydoc !!

.. py:property:: nmetis
   :type: int


   
   Get or set the Number of times to use Metis on each compute node for MPP.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_ORDERING'






