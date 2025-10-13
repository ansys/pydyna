





:class:`FatigueMultiaxial`
==========================


.. py:class:: fatigue_multiaxial.FatigueMultiaxial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FATIGUE_MULTIAXIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FatigueMultiaxial

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~maxial`
            - Get or set the Multiaxial fatigue analysis criterion:
          * - :py:attr:`~nplane`
            - Get or set the Number of planes for fatigue analysis (for MAXIAL = 1 only)


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

    from fatigue_multiaxial import FatigueMultiaxial

Property detail
---------------

.. py:property:: maxial
   :type: int


   
   Get or set the Multiaxial fatigue analysis criterion:
   EQ.0: fatigue analysis using equivalent stress or strain index (defined by INDEX in *FATIGUE)
   EQ.1: fatigue analysis on multiple planes
   EQ.2: fatigue analysis on critical plane which is determined    by the highest 1st principal stress or strain
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: int


   
   Get or set the Number of planes for fatigue analysis (for MAXIAL = 1 only)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FATIGUE'


.. py:attribute:: subkeyword
   :value: 'MULTIAXIAL'






