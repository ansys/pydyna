





:class:`LoadSeismicSsiAuxId`
============================


.. py:class:: load_seismic_ssi_aux_id.LoadSeismicSsiAuxId(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_SEISMIC_SSI_AUX_ID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadSeismicSsiAuxId

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the loading ID
          * - :py:attr:`~heading`
            - Get or set the A description of the loading.
          * - :py:attr:`~filename`
            - Get or set the Name of binary file containing recorded motions
          * - :py:attr:`~ssid`
            - Get or set the Soil-structure interface ID.
          * - :py:attr:`~gmset`
            - Get or set the Identifier for set of recorded motions; see *INTERFACE_SSI_AUX or *INTERFACE_SSI_AUX_?EMBEDDED
          * - :py:attr:`~sf`
            - Get or set the Ground motion scale factor.
          * - :py:attr:`~birth`
            - Get or set the Time at which specified ground motion is activated.
          * - :py:attr:`~death`
            - Get or set the Time at which specified ground motion is removed.
          * - :py:attr:`~isg`
            - Get or set the Definition of soil-structure interface:
          * - :py:attr:`~memgm`
            - Get or set the Size in words of buffer allocated to read in recorded motions


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

    from load_seismic_ssi_aux_id import LoadSeismicSsiAuxId

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the loading ID
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the A description of the loading.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Name of binary file containing recorded motions
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Soil-structure interface ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmset
   :type: Optional[int]


   
   Get or set the Identifier for set of recorded motions; see *INTERFACE_SSI_AUX or *INTERFACE_SSI_AUX_?EMBEDDED
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Ground motion scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time at which specified ground motion is activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time at which specified ground motion is removed.
















   ..
       !! processed by numpydoc !!

.. py:property:: isg
   :type: int


   
   Get or set the Definition of soil-structure interface:
   EQ.0: SSID is the ID for the soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.For the DECONV keyword option, ISG = 0 additionally flags that the free-field within motion is computed at depth
   EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure.For the DECONV, ISG = 1 additionally flags that the free-field outcrop motion is computed at depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: memgm
   :type: int


   
   Get or set the Size in words of buffer allocated to read in recorded motions
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'SEISMIC_SSI_AUX_ID'






