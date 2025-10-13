





:class:`IncludeStampedPartSolidToSolid`
=======================================


.. py:class:: include_stamped_part_solid_to_solid.IncludeStampedPartSolidToSolid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_STAMPED_PART_SOLID_TO_SOLID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeStampedPartSolidToSolid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the File name of the dynain file to be included to map the results from, with maximum of 80 characters.
          * - :py:attr:`~pid`
            - Get or set the Part ID of the target part onto which the source model’s results in FILENAME will be mapped.
          * - :py:attr:`~thick`
            - Get or set the Flag to map part thickness.  The thickness direction is determined from the element normals, hence the need for consistency in the element normals.  The thicknesses of the target part elements are adjusted so the target part thickness matches the source part thickness at any given location.  Currently, this variable is hardwired so that thickness mapping is always on.
          * - :py:attr:`~pstrn`
            - Get or set the Flag to map effective plastic strain.  Currently setting this flag with any integer will map the effective plastic strain, and there is no other option.
          * - :py:attr:`~strain`
            - Get or set the Flag to map the strain tensor.  Currently setting this flag with any integer will map the tensorial strains, and there is no other option available.  Note “STRFLG” in *DATABASE_‌EXTENT_‌BINARY must be set to “1” for output to d3plot as well as dynain files.
          * - :py:attr:`~stress`
            - Get or set the Flag to map stress tensor.  Currently setting this flag with any integer will map the stresses and history variables, and there is no other option available.  Only the history variables included in the dynain file specified by FILENAME are mapped; see “NSHV” in *INTERFACE_‌SPRINGBACK_‌LSDYNA for control of history variable output to dynain.
          * - :py:attr:`~n1sorc`
            - Get or set the First of 3 nodes needed to reorient the source part.  No transformation if undefined.
          * - :py:attr:`~n2sorc`
            - Get or set the Second of 3 nodes needed to reorient the source part.  No transformation if undefined
          * - :py:attr:`~n3sorc`
            - Get or set the Third of 3 nodes needed to reorient the source part.  No transformation if undefined.
          * - :py:attr:`~n1trgt`
            - Get or set the First of 3 nodes needed to reorient the target part.  No transformation if undefined.
          * - :py:attr:`~n2trgt`
            - Get or set the Second of 3 nodes needed to reorient the target part.  No transformation if undefined
          * - :py:attr:`~n3trgt`
            - Get or set the Third of 3 nodes needed to reorient the target part.  No transformation if undefined


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

    from include_stamped_part_solid_to_solid import IncludeStampedPartSolidToSolid

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the File name of the dynain file to be included to map the results from, with maximum of 80 characters.
   This should be the native dynain file containing the results from metal stamping,
   directly from a LS-DYNA simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of the target part onto which the source model’s results in FILENAME will be mapped.
   LT.0:   part ID of the target part is | PID | and the normals of the target part are flipped before mapping.A negative PID would be used if the target part’s normals were oriented exactly opposite those of the source part.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: int


   
   Get or set the Flag to map part thickness.  The thickness direction is determined from the element normals, hence the need for consistency in the element normals.  The thicknesses of the target part elements are adjusted so the target part thickness matches the source part thickness at any given location.  Currently, this variable is hardwired so that thickness mapping is always on.
















   ..
       !! processed by numpydoc !!

.. py:property:: pstrn
   :type: int


   
   Get or set the Flag to map effective plastic strain.  Currently setting this flag with any integer will map the effective plastic strain, and there is no other option.
   EQ.0:   map effective plastic strain.
   NE.0 : do not map effective plastic strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: strain
   :type: int


   
   Get or set the Flag to map the strain tensor.  Currently setting this flag with any integer will map the tensorial strains, and there is no other option available.  Note “STRFLG” in *DATABASE_‌EXTENT_‌BINARY must be set to “1” for output to d3plot as well as dynain files.
   EQ.0:   map strain tensor.
   NE.0 : do not map strain tensor
















   ..
       !! processed by numpydoc !!

.. py:property:: stress
   :type: int


   
   Get or set the Flag to map stress tensor.  Currently setting this flag with any integer will map the stresses and history variables, and there is no other option available.  Only the history variables included in the dynain file specified by FILENAME are mapped; see “NSHV” in *INTERFACE_‌SPRINGBACK_‌LSDYNA for control of history variable output to dynain.
   EQ.0:   map stress tensor and history variables.
   NE.0 : do not map stress tensorand history variables
















   ..
       !! processed by numpydoc !!

.. py:property:: n1sorc
   :type: int


   
   Get or set the First of 3 nodes needed to reorient the source part.  No transformation if undefined.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2sorc
   :type: int


   
   Get or set the Second of 3 nodes needed to reorient the source part.  No transformation if undefined
















   ..
       !! processed by numpydoc !!

.. py:property:: n3sorc
   :type: int


   
   Get or set the Third of 3 nodes needed to reorient the source part.  No transformation if undefined.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1trgt
   :type: int


   
   Get or set the First of 3 nodes needed to reorient the target part.  No transformation if undefined.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2trgt
   :type: int


   
   Get or set the Second of 3 nodes needed to reorient the target part.  No transformation if undefined
















   ..
       !! processed by numpydoc !!

.. py:property:: n3trgt
   :type: int


   
   Get or set the Third of 3 nodes needed to reorient the target part.  No transformation if undefined
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'STAMPED_PART_SOLID_TO_SOLID'






