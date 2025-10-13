





:class:`InterfaceSsiAuxEmbedded`
================================


.. py:class:: interface_ssi_aux_embedded.InterfaceSsiAuxEmbedded(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_SSI_AUX_EMBEDDED keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceSsiAuxEmbedded

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
          * - :py:attr:`~gmset`
            - Get or set the Identifier for this set of recorded motions to be referred to in *INTERFACE_SSI. Must be unique.
          * - :py:attr:`~strid`
            - Get or set the Segment set ID of base of structure at soil-structure interface.
          * - :py:attr:`~soilid`
            - Get or set the Segment set ID of soil at soil-structure interface.
          * - :py:attr:`~spr`
            - Get or set the Include the slave side in the *DATABASE_NCFORC and the
          * - :py:attr:`~mpr`
            - Get or set the Include the master side in the *DATABASE_NCFORC and the


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

    from interface_ssi_aux_embedded import InterfaceSsiAuxEmbedded

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

.. py:property:: gmset
   :type: Optional[int]


   
   Get or set the Identifier for this set of recorded motions to be referred to in *INTERFACE_SSI. Must be unique.
















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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'SSI_AUX_EMBEDDED'






