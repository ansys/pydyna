





:class:`InitialVolumeFractionNalegp`
====================================


.. py:class:: initial_volume_fraction_nalegp.InitialVolumeFractionNalegp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_VOLUME_FRACTION_NALEGP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialVolumeFractionNalegp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nalegp`
            - Get or set the Count of volume fractions for each element.
          * - :py:attr:`~eid`
            - Get or set the Element ID, see also *ELEMENT_OPTION.
          * - :py:attr:`~vf1`
            - Get or set the Volume fraction of multi-material group 1.
          * - :py:attr:`~vf2`
            - Get or set the Volume fraction of multi-material group 2.
          * - :py:attr:`~vf3`
            - Get or set the Volume fraction of multi-material group 3, AMMGID=3.
          * - :py:attr:`~vf4`
            - Get or set the Volume fraction of multi-material group 4, AMMGID=4.
          * - :py:attr:`~vf5`
            - Get or set the Volume fraction of multi-material group 5, AMMGID=5.
          * - :py:attr:`~vf6`
            - Get or set the Volume fraction of multi-material group 6, AMMGID=6.
          * - :py:attr:`~vf7`
            - Get or set the Volume fraction of multi-material group 7, AMMGID=7.
          * - :py:attr:`~vf`
            - Get or set the Volume fraction of multi-material group.


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

    from initial_volume_fraction_nalegp import InitialVolumeFractionNalegp

Property detail
---------------

.. py:property:: nalegp
   :type: Optional[int]


   
   Get or set the Count of volume fractions for each element.
















   ..
       !! processed by numpydoc !!

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID, see also *ELEMENT_OPTION.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf1
   :type: float


   
   Get or set the Volume fraction of multi-material group 1.
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf2
   :type: float


   
   Get or set the Volume fraction of multi-material group 2.
   Only needed in simulations with 3 material groups. Otherwise VF2=1-VF1.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf3
   :type: float


   
   Get or set the Volume fraction of multi-material group 3, AMMGID=3.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf4
   :type: float


   
   Get or set the Volume fraction of multi-material group 4, AMMGID=4.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf5
   :type: float


   
   Get or set the Volume fraction of multi-material group 5, AMMGID=5.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf6
   :type: float


   
   Get or set the Volume fraction of multi-material group 6, AMMGID=6.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf7
   :type: float


   
   Get or set the Volume fraction of multi-material group 7, AMMGID=7.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf
   :type: float


   
   Get or set the Volume fraction of multi-material group.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'VOLUME_FRACTION_NALEGP'






