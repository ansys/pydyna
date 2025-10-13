





:class:`InterfaceSsi`
=====================


.. py:class:: interface_ssi.InterfaceSsi(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_SSI keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceSsi

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Soil-structure interface ID. This is required and must be unique amongst all the contact interface IDs in the model.
          * - :py:attr:`~heading`
            - Get or set the A descriptor for the given ID.
          * - :py:attr:`~strid`
            - Get or set the Segment set ID of base of structure at soil-structure interface.
          * - :py:attr:`~soilid`
            - Get or set the Segment set ID of soil at soil-structure interface.
          * - :py:attr:`~spr`
            - Get or set the Include the slave side in the *DATABASE_NCFORC and the
          * - :py:attr:`~mpr`
            - Get or set the Include the master side in the *DATABASE_NCFORC and the
          * - :py:attr:`~gmset`
            - Get or set the Identifier for set of recorded motions from *INTERFACE_SSI_AUX or *INTERFACE_SSI_AUX_EMBEDDED.
          * - :py:attr:`~sf`
            - Get or set the Recorded motion scale factor. (default = 1.0).
          * - :py:attr:`~birth`
            - Get or set the Time at which specified recorded motion is activated.
          * - :py:attr:`~death`
            - Get or set the Time at which specified recorded motion is removed:      EQ.0.0: default set to 1028.
          * - :py:attr:`~memgm`
            - Get or set the Size in words of buffer allocated to read in recorded motions.


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

    from interface_ssi import InterfaceSsi

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Soil-structure interface ID. This is required and must be unique amongst all the contact interface IDs in the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: heading
   :type: Optional[str]


   
   Get or set the A descriptor for the given ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: strid
   :type: Optional[int]


   
   Get or set the Segment set ID of base of structure at soil-structure interface.
















   ..
       !! processed by numpydoc !!

.. py:property:: soilid
   :type: Optional[int]


   
   Get or set the Segment set ID of soil at soil-structure interface.
















   ..
       !! processed by numpydoc !!

.. py:property:: spr
   :type: Optional[int]


   
   Get or set the Include the slave side in the *DATABASE_NCFORC and the
   *DATABASE_BINARY_INTFOR interface force files:
   EQ.1: slave side forces included.
















   ..
       !! processed by numpydoc !!

.. py:property:: mpr
   :type: Optional[int]


   
   Get or set the Include the master side in the *DATABASE_NCFORC and the
   *DATABASE_BINARY_INTFOR interface force files:
   EQ.1: master side forces included.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmset
   :type: Optional[int]


   
   Get or set the Identifier for set of recorded motions from *INTERFACE_SSI_AUX or *INTERFACE_SSI_AUX_EMBEDDED.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Recorded motion scale factor. (default = 1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time at which specified recorded motion is activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time at which specified recorded motion is removed:      EQ.0.0: default set to 1028.
















   ..
       !! processed by numpydoc !!

.. py:property:: memgm
   :type: int


   
   Get or set the Size in words of buffer allocated to read in recorded motions.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'SSI'






