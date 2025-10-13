





:class:`InterfaceBlanksizeInitialAdaptive`
==========================================


.. py:class:: interface_blanksize_initial_adaptive.InterfaceBlanksizeInitialAdaptive(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_BLANKSIZE_INITIAL_ADAPTIVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceBlanksizeInitialAdaptive

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename9`
            - Get or set the The following file names, FILENAME8~11 are for the option INITIAL_ADAPTIVE:
          * - :py:attr:`~filename10`
            - Get or set the OP30 initial blank file name, from last state of OP30 D3PLOTS with adaptive
          * - :py:attr:`~filename11`
            - Get or set the OP30 adapted initial blank file name. For now, this can be extracted from "adapt.msh" file of OP30 simulation.
          * - :py:attr:`~filename12`
            - Get or set the OP30 flat blank (calculated) file name. For example, it can be "op30_flat_new".


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

    from interface_blanksize_initial_adaptive import InterfaceBlanksizeInitialAdaptive

Property detail
---------------

.. py:property:: filename9
   :type: Optional[str]


   
   Get or set the The following file names, FILENAME8~11 are for the option INITIAL_ADAPTIVE:
   Same file name as FILENAME7, calculated.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename10
   :type: Optional[str]


   
   Get or set the OP30 initial blank file name, from last state of OP30 D3PLOTS with adaptive
   constraints, or otherwise the same as OP20 dynain. To get the file from
   D3PLOTS, access POST/OUTPUT/Dynain ASCII and Exclude strain and
   stress in LS-PrePost4.0. Typically, OP30 will be a flanging operation.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename11
   :type: Optional[str]


   
   Get or set the OP30 adapted initial blank file name. For now, this can be extracted from "adapt.msh" file of OP30 simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename12
   :type: Optional[str]


   
   Get or set the OP30 flat blank (calculated) file name. For example, it can be "op30_flat_new".
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'BLANKSIZE_INITIAL_ADAPTIVE'






