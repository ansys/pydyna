





:class:`PartAdaptiveFailure`
============================


.. py:class:: part_adaptive_failure.PartAdaptiveFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_ADAPTIVE_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartAdaptiveFailure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID
          * - :py:attr:`~t`
            - Get or set the Thickness. When the thickness of the part reaches this minimum value the part is split into two parts. The value for T should be on the order of the element thickness of a typical element.
          * - :py:attr:`~term`
            - Get or set the Control adaptivity after the part separates:


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

    from part_adaptive_failure import PartAdaptiveFailure

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID
















   ..
       !! processed by numpydoc !!

.. py:property:: t
   :type: Optional[float]


   
   Get or set the Thickness. When the thickness of the part reaches this minimum value the part is split into two parts. The value for T should be on the order of the element thickness of a typical element.
















   ..
       !! processed by numpydoc !!

.. py:property:: term
   :type: int


   
   Get or set the Control adaptivity after the part separates:
   EQ.0:   continue to adapt part(default)
   EQ.1 : remove only this part from the adaptivity.Other parts will continue to adapt as normal.If there are no remaining parts to be adapted, adaptivity is disabled
   EQ.2 : adaptivity is disabled for all parts.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'ADAPTIVE_FAILURE'






