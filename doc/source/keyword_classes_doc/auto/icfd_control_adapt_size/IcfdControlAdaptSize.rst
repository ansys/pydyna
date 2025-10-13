





:class:`IcfdControlAdaptSize`
=============================


.. py:class:: icfd_control_adapt_size.IcfdControlAdaptSize(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_ADAPT_SIZE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlAdaptSize

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~asize`
            - Get or set the EQ. 0:only re-mesh in cases where elements invert..
          * - :py:attr:`~nit`
            - Get or set the Number of iterations before a re-meshing is forced.


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

    from icfd_control_adapt_size import IcfdControlAdaptSize

Property detail
---------------

.. py:property:: asize
   :type: int


   
   Get or set the EQ. 0:only re-mesh in cases where elements invert..
   EQ. 1:re-mesh if elements invert or if element quality deteriorates.
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: nit
   :type: Optional[int]


   
   Get or set the Number of iterations before a re-meshing is forced.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_ADAPT_SIZE'






