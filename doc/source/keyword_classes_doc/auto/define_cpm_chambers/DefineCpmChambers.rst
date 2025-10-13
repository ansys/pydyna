





:class:`DefineCpmChambers`
==========================


.. py:class:: define_cpm_chambers.DefineCpmChambers(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CPM_CHAMBERS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCpmChambers

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Unique ID for this card
          * - :py:attr:`~nchm`
            - Get or set the Number of chambers defined
          * - :py:attr:`~sid1`
            - Get or set the Part set defining the chamber (normals pointed inward)
          * - :py:attr:`~sid2`
            - Get or set the Part set defining the chamber (normals pointed outward)
          * - :py:attr:`~ninter`
            - Get or set the Number of vent hole definition for chamber interaction
          * - :py:attr:`~chm_id`
            - Get or set the Chamber ID
          * - :py:attr:`~sid3`
            - Get or set the Set defining interaction between chambers
          * - :py:attr:`~itype3`
            - Get or set the Set type EQ.0: Part
          * - :py:attr:`~tochm`
            - Get or set the The chamber ID of the connected chamber
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

    from define_cpm_chambers import DefineCpmChambers

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Unique ID for this card
















   ..
       !! processed by numpydoc !!

.. py:property:: nchm
   :type: int


   
   Get or set the Number of chambers defined
















   ..
       !! processed by numpydoc !!

.. py:property:: sid1
   :type: Optional[int]


   
   Get or set the Part set defining the chamber (normals pointed inward)
















   ..
       !! processed by numpydoc !!

.. py:property:: sid2
   :type: int


   
   Get or set the Part set defining the chamber (normals pointed outward)
















   ..
       !! processed by numpydoc !!

.. py:property:: ninter
   :type: int


   
   Get or set the Number of vent hole definition for chamber interaction
















   ..
       !! processed by numpydoc !!

.. py:property:: chm_id
   :type: int


   
   Get or set the Chamber ID
















   ..
       !! processed by numpydoc !!

.. py:property:: sid3
   :type: Optional[int]


   
   Get or set the Set defining interaction between chambers
















   ..
       !! processed by numpydoc !!

.. py:property:: itype3
   :type: int


   
   Get or set the Set type EQ.0: Part
   EQ.1: Part set
















   ..
       !! processed by numpydoc !!

.. py:property:: tochm
   :type: Optional[int]


   
   Get or set the The chamber ID of the connected chamber
















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
   :value: 'CPM_CHAMBERS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





