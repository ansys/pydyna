





:class:`ControlImplicitConsistentMass`
======================================


.. py:class:: control_implicit_consistent_mass.ControlImplicitConsistentMass(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_IMPLICIT_CONSISTENT_MASS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlImplicitConsistentMass

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~iflag`
            - Get or set the Consistent mass matrix flag


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

    from control_implicit_consistent_mass import ControlImplicitConsistentMass

Property detail
---------------

.. py:property:: iflag
   :type: int


   
   Get or set the Consistent mass matrix flag
   EQ.0: Use the standard lumped mass formulation (DEFAULT)
   EQ.1: Use the consistent mass matrix
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'IMPLICIT_CONSISTENT_MASS'






