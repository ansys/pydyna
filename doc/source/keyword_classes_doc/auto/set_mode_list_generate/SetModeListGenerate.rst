





:class:`SetModeListGenerate`
============================


.. py:class:: set_mode_list_generate.SetModeListGenerate(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_MODE_LIST_GENERATE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetModeListGenerate

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Set identification. All mode sets should have a unique set ID.
          * - :py:attr:`~m1beg`
            - Get or set the First mode ID in block 1.
          * - :py:attr:`~m1end`
            - Get or set the Last mode ID in block 1. All defined IDs between and including MnBEG and MnEND are added to the set.
          * - :py:attr:`~m2beg`
            - Get or set the First mode ID in block 2.
          * - :py:attr:`~m2end`
            - Get or set the Last mode ID in block 2. All defined IDs between and including MnBEG and MnEND are added to the set.
          * - :py:attr:`~m3beg`
            - Get or set the First mode ID in block 3.
          * - :py:attr:`~m3end`
            - Get or set the Last mode ID in block 3. All defined IDs between and including MnBEG and MnEND are added to the set.
          * - :py:attr:`~m4beg`
            - Get or set the First mode ID in block 4.
          * - :py:attr:`~m4end`
            - Get or set the Last mode ID in block 4. All defined IDs between and including MnBEG and MnEND are added to the set.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from set_mode_list_generate import SetModeListGenerate

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Set identification. All mode sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: m1beg
   :type: Optional[int]


   
   Get or set the First mode ID in block 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: m1end
   :type: Optional[int]


   
   Get or set the Last mode ID in block 1. All defined IDs between and including MnBEG and MnEND are added to the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: m2beg
   :type: Optional[int]


   
   Get or set the First mode ID in block 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: m2end
   :type: Optional[int]


   
   Get or set the Last mode ID in block 2. All defined IDs between and including MnBEG and MnEND are added to the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: m3beg
   :type: Optional[int]


   
   Get or set the First mode ID in block 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: m3end
   :type: Optional[int]


   
   Get or set the Last mode ID in block 3. All defined IDs between and including MnBEG and MnEND are added to the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: m4beg
   :type: Optional[int]


   
   Get or set the First mode ID in block 4.
















   ..
       !! processed by numpydoc !!

.. py:property:: m4end
   :type: Optional[int]


   
   Get or set the Last mode ID in block 4. All defined IDs between and including MnBEG and MnEND are added to the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'MODE_LIST_GENERATE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





