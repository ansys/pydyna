





:class:`InterfaceSsiStatic`
===========================


.. py:class:: interface_ssi_static.InterfaceSsiStatic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_SSI_STATIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceSsiStatic

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

    from interface_ssi_static import InterfaceSsiStatic

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



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'SSI_STATIC'






