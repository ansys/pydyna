





:class:`DatabaseD3Max`
======================


.. py:class:: database_d3max.DatabaseD3Max(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_D3MAX keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseD3Max

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~dtcheck`
            - Get or set the Time step for checking and updating maximum values. For instance, if DTCHECK = 10-6, LS-DYNA will check and update the maximum values every 10-6 seconds (assuming for this example the time units are seconds). It will compare the current values (stress or strain) with the maximum values up to now. If the current values are larger, the maximum values will be replaced by the current values. Otherwise, the maximum values will remain unchanged
          * - :py:attr:`~me`
            - Get or set the Method for extraction of stresses:
          * - :py:attr:`~pstrs`
            - Get or set the Output principal stress:
          * - :py:attr:`~pstrn`
            - Get or set the Output principal strain:
          * - :py:attr:`~ifilt`
            - Get or set the Use filter:
          * - :py:attr:`~output`
            - Get or set the Output format:
          * - :py:attr:`~fcutout`
            - Get or set the Cutout frequency for Butterworth filter


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

    from database_d3max import DatabaseD3Max

Property detail
---------------

.. py:property:: dtcheck
   :type: Optional[float]


   
   Get or set the Time step for checking and updating maximum values. For instance, if DTCHECK = 10-6, LS-DYNA will check and update the maximum values every 10-6 seconds (assuming for this example the time units are seconds). It will compare the current values (stress or strain) with the maximum values up to now. If the current values are larger, the maximum values will be replaced by the current values. Otherwise, the maximum values will remain unchanged
















   ..
       !! processed by numpydoc !!

.. py:property:: me
   :type: int


   
   Get or set the Method for extraction of stresses:
   EQ.1:   extracting max stress / strain during transient analysis.
   EQ.2 : extracting max stress / strain after transient analysis(not used)
















   ..
       !! processed by numpydoc !!

.. py:property:: pstrs
   :type: int


   
   Get or set the Output principal stress:
   EQ.0:   no
   EQ.1 : yes
















   ..
       !! processed by numpydoc !!

.. py:property:: pstrn
   :type: int


   
   Get or set the Output principal strain:
   EQ.0:   no
   EQ.1 : yes
















   ..
       !! processed by numpydoc !!

.. py:property:: ifilt
   :type: int


   
   Get or set the Use filter:
   EQ.0:   no
   EQ.1 : use low pass 2nd order Butterworth filter
















   ..
       !! processed by numpydoc !!

.. py:property:: output
   :type: int


   
   Get or set the Output format:
   EQ.0:   Write maximum stress / strain to d3max
   EQ.1 : Append the maximum stress / strain results to d3part
   EQ.2 : Write the maximum stress / strain results to d3part instead of the normal data that goes into d3part(negative time stamps are used in d3part to distinguish when this is done from the normal d3part output, which saves time history results for selected parts)
















   ..
       !! processed by numpydoc !!

.. py:property:: fcutout
   :type: float


   
   Get or set the Cutout frequency for Butterworth filter
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'D3MAX'






