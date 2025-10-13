





:class:`ChemistryModel`
=======================


.. py:class:: chemistry_model.ChemistryModel(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_MODEL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryModel

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~modelid`
            - Get or set the Identifier for this chemkin-based chemistry model.
          * - :py:attr:`~jacsel`
            - Get or set the Selects the form of the Jacobian matrix for use in the source term.
          * - :py:attr:`~errlim`
            - Get or set the Allowed error in element balance in a chemical reaction.
          * - :py:attr:`~file1`
            - Get or set the Name of the file containing the Chemkin-compatible input.
          * - :py:attr:`~file2`
            - Get or set the Name of the file containing the chemistry thermodynamics database.
          * - :py:attr:`~file3`
            - Get or set the Name of the file containing the chemistry transport properties database.


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

    from chemistry_model import ChemistryModel

Property detail
---------------

.. py:property:: modelid
   :type: Optional[int]


   
   Get or set the Identifier for this chemkin-based chemistry model.
















   ..
       !! processed by numpydoc !!

.. py:property:: jacsel
   :type: int


   
   Get or set the Selects the form of the Jacobian matrix for use in the source term.
   EQ.1:   Fully implicit(default)
   EQ.2 : Simplified implicit
















   ..
       !! processed by numpydoc !!

.. py:property:: errlim
   :type: float


   
   Get or set the Allowed error in element balance in a chemical reaction.
















   ..
       !! processed by numpydoc !!

.. py:property:: file1
   :type: Optional[str]


   
   Get or set the Name of the file containing the Chemkin-compatible input.
















   ..
       !! processed by numpydoc !!

.. py:property:: file2
   :type: Optional[str]


   
   Get or set the Name of the file containing the chemistry thermodynamics database.
















   ..
       !! processed by numpydoc !!

.. py:property:: file3
   :type: Optional[str]


   
   Get or set the Name of the file containing the chemistry transport properties database.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'MODEL'






