





:class:`ChemistryControlHgiPart`
================================


.. py:class:: chemistry_control_hgi_part.ChemistryControlHgiPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_HGI_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControlHgiPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identifier for this chemistry solver.
          * - :py:attr:`~compid`
            - Get or set the Chemical composition identifier of the initial composition
          * - :py:attr:`~exit_bc`
            - Get or set the The exit boundary a surface part ID referenced in *MESH_‌SURFACE_‌ELEMENT cards (for the PART option).
          * - :py:attr:`~file`
            - Get or set the Name of the lsda file in which to write the results of the inflator simulation.


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

    from chemistry_control_hgi_part import ChemistryControlHgiPart

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identifier for this chemistry solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: compid
   :type: Optional[int]


   
   Get or set the Chemical composition identifier of the initial composition
















   ..
       !! processed by numpydoc !!

.. py:property:: exit_bc
   :type: Optional[int]


   
   Get or set the The exit boundary a surface part ID referenced in *MESH_‌SURFACE_‌ELEMENT cards (for the PART option).
















   ..
       !! processed by numpydoc !!

.. py:property:: file
   :type: Optional[str]


   
   Get or set the Name of the lsda file in which to write the results of the inflator simulation.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'CONTROL_HGI_PART'






