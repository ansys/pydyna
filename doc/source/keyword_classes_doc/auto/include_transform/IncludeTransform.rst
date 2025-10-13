





:class:`IncludeTransform`
=========================


.. py:class:: include_transform.IncludeTransform(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_TRANSFORM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeTransform

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the File name of file to be included in this keyword file.
          * - :py:attr:`~idnoff`
            - Get or set the Offset to node ID.
          * - :py:attr:`~ideoff`
            - Get or set the Offset to element ID.
          * - :py:attr:`~idpoff`
            - Get or set the Offset to part ID.
          * - :py:attr:`~idmoff`
            - Get or set the Offset to material ID.
          * - :py:attr:`~idsoff`
            - Get or set the Offset to set ID.
          * - :py:attr:`~idfoff`
            - Get or set the Offset to function ID, table ID, and curve ID.
          * - :py:attr:`~iddoff`
            - Get or set the Offset to any ID defined through *DEFINE except the FUNCTION, TABLE, and CURVE options (see IDFOFF).
          * - :py:attr:`~idroff`
            - Get or set the Used for all offsets except for those listed above.
          * - :py:attr:`~prefix`
            - Get or set the Prefix added to the beginning of the titles/heads defined in the keywords (like *MAT, *PART, *SECTION, *DEFINE, for examples) of the included file.  A dot, "." is automatically added between the prefix and the existing title
          * - :py:attr:`~suffix`
            - Get or set the Suffix added to the end of the titles/heads defined in the keywords of the included file.  A dot, "." is automatically added between the suffix and the existing title
          * - :py:attr:`~fctmas`
            - Get or set the Mass transformation factor. For example, FCTMAS=1000.0 when the original mass units are in tons and the new unit is kg.
          * - :py:attr:`~fcttim`
            - Get or set the Time transformation factor. For example, FCTTIM=0.001 when the original time units are in milliseconds and the new time unit is seconds.
          * - :py:attr:`~fctlen`
            - Get or set the Length transformation factor.
          * - :py:attr:`~fcttem`
            - Get or set the Temperature transformation factor: F to C (Farenheit to Centigrade), C to F, F to K, K to F, and so on.
          * - :py:attr:`~incout1`
            - Get or set the Set to 1 for the creation of a file, DYNA.INC, which contains the transformed data. The data in this file can be used in future include files and should be checked to ensure that all the data was transformed correctly.
          * - :py:attr:`~fctchg`
            - Get or set the Electric charge transformation factor, currently only applied to piezoelectric material related cards, see *MAT_ADD_PZELECTRIC for details.
          * - :py:attr:`~tranid`
            - Get or set the Transformation ID.
          * - :py:attr:`~tranid_link`
            - Get the DefineTransformation object for tranid.


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

    from include_transform import IncludeTransform

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the File name of file to be included in this keyword file.
   Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
















   ..
       !! processed by numpydoc !!

.. py:property:: idnoff
   :type: int


   
   Get or set the Offset to node ID.
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: ideoff
   :type: int


   
   Get or set the Offset to element ID.
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: idpoff
   :type: int


   
   Get or set the Offset to part ID.
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: idmoff
   :type: int


   
   Get or set the Offset to material ID.
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: idsoff
   :type: int


   
   Get or set the Offset to set ID.
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: idfoff
   :type: int


   
   Get or set the Offset to function ID, table ID, and curve ID.
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: iddoff
   :type: int


   
   Get or set the Offset to any ID defined through *DEFINE except the FUNCTION, TABLE, and CURVE options (see IDFOFF).
   Default is set to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: idroff
   :type: int


   
   Get or set the Used for all offsets except for those listed above.
















   ..
       !! processed by numpydoc !!

.. py:property:: prefix
   :type: Optional[str]


   
   Get or set the Prefix added to the beginning of the titles/heads defined in the keywords (like *MAT, *PART, *SECTION, *DEFINE, for examples) of the included file.  A dot, "." is automatically added between the prefix and the existing title
















   ..
       !! processed by numpydoc !!

.. py:property:: suffix
   :type: Optional[str]


   
   Get or set the Suffix added to the end of the titles/heads defined in the keywords of the included file.  A dot, "." is automatically added between the suffix and the existing title
















   ..
       !! processed by numpydoc !!

.. py:property:: fctmas
   :type: float


   
   Get or set the Mass transformation factor. For example, FCTMAS=1000.0 when the original mass units are in tons and the new unit is kg.
















   ..
       !! processed by numpydoc !!

.. py:property:: fcttim
   :type: float


   
   Get or set the Time transformation factor. For example, FCTTIM=0.001 when the original time units are in milliseconds and the new time unit is seconds.
















   ..
       !! processed by numpydoc !!

.. py:property:: fctlen
   :type: float


   
   Get or set the Length transformation factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: fcttem
   :type: str


   
   Get or set the Temperature transformation factor: F to C (Farenheit to Centigrade), C to F, F to K, K to F, and so on.
















   ..
       !! processed by numpydoc !!

.. py:property:: incout1
   :type: int


   
   Get or set the Set to 1 for the creation of a file, DYNA.INC, which contains the transformed data. The data in this file can be used in future include files and should be checked to ensure that all the data was transformed correctly.
















   ..
       !! processed by numpydoc !!

.. py:property:: fctchg
   :type: Optional[float]


   
   Get or set the Electric charge transformation factor, currently only applied to piezoelectric material related cards, see *MAT_ADD_PZELECTRIC for details.
















   ..
       !! processed by numpydoc !!

.. py:property:: tranid
   :type: int


   
   Get or set the Transformation ID.
   EQ.0: no tranformation will be applied.  See the input *DEFINE_TRANSFORM.
















   ..
       !! processed by numpydoc !!

.. py:property:: tranid_link
   :type: define_transformation.DefineTransformation


   
   Get the DefineTransformation object for tranid.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'TRANSFORM'






