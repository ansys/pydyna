





:class:`AleFragmentation`
=========================


.. py:class:: ale_fragmentation.AleFragmentation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_FRAGMENTATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleFragmentation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fr_mmg`
            - Get or set the This is the AMMGID of the failed material
          * - :py:attr:`~to_mmg`
            - Get or set the This is the AMMGID of the vacuum to which the failed material is being switched
          * - :py:attr:`~fragtyp`
            - Get or set the Flag defining whether the failed material is completely or partially switched to vacuum.


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

    from ale_fragmentation import AleFragmentation

Property detail
---------------

.. py:property:: fr_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMGID of the failed material
















   ..
       !! processed by numpydoc !!

.. py:property:: to_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMGID of the vacuum to which the failed material is being switched
















   ..
       !! processed by numpydoc !!

.. py:property:: fragtyp
   :type: int


   
   Get or set the Flag defining whether the failed material is completely or partially switched to vacuum.
   EQ.1:   Fully switch; all failed material is switched to vacuum.
   EQ.2:   Partially switch; only the volume expansion from the last time step is switched to vacuum
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'FRAGMENTATION'






