





:class:`IncludeNastran`
=======================


.. py:class:: include_nastran.IncludeNastran(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_NASTRAN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeNastran

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the File name of file to be included in this keyword file.
          * - :py:attr:`~beamdf`
            - Get or set the LS-DYNA beam element type. Defaults to type 2.
          * - :py:attr:`~shelldf`
            - Get or set the LS-DYNA shell element type. Defaults to type 21.
          * - :py:attr:`~soliddf`
            - Get or set the LS-DYNA solid element type. Defaults to type 18.


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

    from include_nastran import IncludeNastran

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the File name of file to be included in this keyword file.
   Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
















   ..
       !! processed by numpydoc !!

.. py:property:: beamdf
   :type: int


   
   Get or set the LS-DYNA beam element type. Defaults to type 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: shelldf
   :type: int


   
   Get or set the LS-DYNA shell element type. Defaults to type 21.
















   ..
       !! processed by numpydoc !!

.. py:property:: soliddf
   :type: int


   
   Get or set the LS-DYNA solid element type. Defaults to type 18.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'NASTRAN'






