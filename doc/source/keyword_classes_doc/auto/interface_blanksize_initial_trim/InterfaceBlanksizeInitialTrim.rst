





:class:`InterfaceBlanksizeInitialTrim`
======================================


.. py:class:: interface_blanksize_initial_trim.InterfaceBlanksizeInitialTrim(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_BLANKSIZE_INITIAL_TRIM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceBlanksizeInitialTrim

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename5`
            - Get or set the The following file names, FILENAME4~7 are for the option INITIAL_TRIM:
          * - :py:attr:`~filename6`
            - Get or set the OP10 final blank in keyword format. �Dynain file from this OP10 simulation can be used.
          * - :py:attr:`~filename7`
            - Get or set the OP20 blank in keyword format. �Dynain file from this OP20 simulation can be used. Typically, OP20 is a trimming operation.
          * - :py:attr:`~filename8`
            - Get or set the OP20 flat blank (calculated) file name. For example, it can be "op20_flat_new".


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

    from interface_blanksize_initial_trim import InterfaceBlanksizeInitialTrim

Property detail
---------------

.. py:property:: filename5
   :type: Optional[str]


   
   Get or set the The following file names, FILENAME4~7 are for the option INITIAL_TRIM:
   OP10 adapted initial flat blank in keyword format. For now, this can be extracted
   from adapt.msh  file. Typically, OP10 can be a draw forming operation..
















   ..
       !! processed by numpydoc !!

.. py:property:: filename6
   :type: Optional[str]


   
   Get or set the OP10 final blank in keyword format. �Dynain file from this OP10 simulation can be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename7
   :type: Optional[str]


   
   Get or set the OP20 blank in keyword format. �Dynain file from this OP20 simulation can be used. Typically, OP20 is a trimming operation.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename8
   :type: Optional[str]


   
   Get or set the OP20 flat blank (calculated) file name. For example, it can be "op20_flat_new".
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'BLANKSIZE_INITIAL_TRIM'






