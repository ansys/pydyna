





:class:`ControlNonlocal`
========================


.. py:class:: control_nonlocal.ControlNonlocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_NONLOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlNonlocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mem`
            - Get or set the Percentage increase of memory allocated for MAT_NONLOCAL option over that required initially. This is for additional storage that may be required due to geometry changes as the calculation proceeds. Generally, a value of 10 should be sufficient.


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

    from control_nonlocal import ControlNonlocal

Property detail
---------------

.. py:property:: mem
   :type: Optional[int]


   
   Get or set the Percentage increase of memory allocated for MAT_NONLOCAL option over that required initially. This is for additional storage that may be required due to geometry changes as the calculation proceeds. Generally, a value of 10 should be sufficient.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'NONLOCAL'






