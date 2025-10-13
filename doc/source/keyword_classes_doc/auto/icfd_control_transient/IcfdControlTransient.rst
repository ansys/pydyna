





:class:`IcfdControlTransient`
=============================


.. py:class:: icfd_control_transient.IcfdControlTransient(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_TRANSIENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlTransient

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~tord`
            - Get or set the Time integration order :
          * - :py:attr:`~fsord`
            - Get or set the Fractional step integration order :


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

    from icfd_control_transient import IcfdControlTransient

Property detail
---------------

.. py:property:: tord
   :type: int


   
   Get or set the Time integration order :
   EQ.0:   Second order.
   EQ.1:   First order.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsord
   :type: int


   
   Get or set the Fractional step integration order :
   EQ.0:   Second order.
   EQ.1:   First order.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_TRANSIENT'






