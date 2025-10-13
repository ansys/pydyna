





:class:`FatigueFailure`
=======================


.. py:class:: fatigue_failure.FatigueFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FATIGUE_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FatigueFailure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ifailure`
            - Get or set the Treatment of elements failed due to fatigue:
          * - :py:attr:`~dratio`
            - Get or set the Threshold value of cumulative damage ratio for an element to be considered failed


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

    from fatigue_failure import FatigueFailure

Property detail
---------------

.. py:property:: ifailure
   :type: int


   
   Get or set the Treatment of elements failed due to fatigue:
   EQ.0:   keep the elements in the model.
   EQ.1:   delete the elements from the model
















   ..
       !! processed by numpydoc !!

.. py:property:: dratio
   :type: float


   
   Get or set the Threshold value of cumulative damage ratio for an element to be considered failed
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FATIGUE'


.. py:attribute:: subkeyword
   :value: 'FAILURE'






