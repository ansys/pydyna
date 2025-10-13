





:class:`DefineCpmBagInteraction`
================================


.. py:class:: define_cpm_bag_interaction.DefineCpmBagInteraction(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CPM_BAG_INTERACTION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCpmBagInteraction

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~bagid1`
            - Get or set the Airbag ID of source CPM particle bag
          * - :py:attr:`~bagid2`
            - Get or set the Airbag ID of sink CV bag switched from CPM bag
          * - :py:attr:`~nspec`
            - Get or set the The location of the 1st gas component from the CPM bag to be filled in the CV bag.
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

    from define_cpm_bag_interaction import DefineCpmBagInteraction

Property detail
---------------

.. py:property:: bagid1
   :type: Optional[int]


   
   Get or set the Airbag ID of source CPM particle bag
















   ..
       !! processed by numpydoc !!

.. py:property:: bagid2
   :type: Optional[int]


   
   Get or set the Airbag ID of sink CV bag switched from CPM bag
















   ..
       !! processed by numpydoc !!

.. py:property:: nspec
   :type: Optional[int]


   
   Get or set the The location of the 1st gas component from the CPM bag to be filled in the CV bag.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CPM_BAG_INTERACTION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





