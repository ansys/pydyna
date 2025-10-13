





:class:`Case`
=============


.. py:class:: case.Case(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CASE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Case

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~caseid`
            - Get or set the Identification number for case.
          * - :py:attr:`~jobid`
            - Get or set the Optional string (no spaces) to be used as the jobid for this case.
          * - :py:attr:`~commands`
            - Get or set the Command line arguments.
          * - :py:attr:`~scid1`
            - Get or set the Subcase ID active for case CASEID.
          * - :py:attr:`~scid2`
            - Get or set the Subcase ID active for case CASEID.
          * - :py:attr:`~scid3`
            - Get or set the Subcase ID active for case CASEID.
          * - :py:attr:`~scid4`
            - Get or set the Subcase ID active for case CASEID.
          * - :py:attr:`~scid5`
            - Get or set the Subcase ID active for case CASEID.
          * - :py:attr:`~scid6`
            - Get or set the Subcase ID active for case CASEID.
          * - :py:attr:`~scid7`
            - Get or set the Subcase ID active for case CASEID.
          * - :py:attr:`~scid8`
            - Get or set the Subcase ID active for case CASEID.


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

    from case import Case

Property detail
---------------

.. py:property:: caseid
   :type: Optional[int]


   
   Get or set the Identification number for case.
















   ..
       !! processed by numpydoc !!

.. py:property:: jobid
   :type: Optional[str]


   
   Get or set the Optional string (no spaces) to be used as the jobid for this case.
   If no JOBID is specified, the string CASEXX is used, where XX is the CASEID in field 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: commands
   :type: Optional[str]


   
   Get or set the Command line arguments.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid1
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid2
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid3
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid4
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid5
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid6
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid7
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!

.. py:property:: scid8
   :type: Optional[int]


   
   Get or set the Subcase ID active for case CASEID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CASE'


.. py:attribute:: subkeyword
   :value: 'CASE'






