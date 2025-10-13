





:class:`ChemistryControl1D`
===========================


.. py:class:: chemistry_control_1d.ChemistryControl1D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_1D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControl1D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identifier for this one-dimensional detonation solution.
          * - :py:attr:`~xyzd`
            - Get or set the Position of the detonation front in the DETDIR direction.
          * - :py:attr:`~detdir`
            - Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
          * - :py:attr:`~csp_sel`
            - Get or set the CSP solver option:
          * - :py:attr:`~file`
            - Get or set the Name of the LSDA file containing the one-dimensional solution.
          * - :py:attr:`~ampl`
            - Get or set the Relative accuracy for the mass fraction of a chemical species in the Chemkin input file.
          * - :py:attr:`~ycut`
            - Get or set the Absolute accuracy for the mass fraction of a chemical species in the Chemkin input file.


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

    from chemistry_control_1d import ChemistryControl1D

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identifier for this one-dimensional detonation solution.
















   ..
       !! processed by numpydoc !!

.. py:property:: xyzd
   :type: Optional[float]


   
   Get or set the Position of the detonation front in the DETDIR direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: detdir
   :type: Optional[int]


   
   Get or set the Detonation propagation direction (1 => X; 2 => Y; 3 => Z)
















   ..
       !! processed by numpydoc !!

.. py:property:: csp_sel
   :type: int


   
   Get or set the CSP solver option:
   EQ.0: Do not use the CSP solver, and ignore the AMPL and YCUT parameters (default).
   GT.0: Use the CSP solver, with the AMPL and YCUT parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: file
   :type: Optional[str]


   
   Get or set the Name of the LSDA file containing the one-dimensional solution.
















   ..
       !! processed by numpydoc !!

.. py:property:: ampl
   :type: Optional[float]


   
   Get or set the Relative accuracy for the mass fraction of a chemical species in the Chemkin input file.
















   ..
       !! processed by numpydoc !!

.. py:property:: ycut
   :type: Optional[float]


   
   Get or set the Absolute accuracy for the mass fraction of a chemical species in the Chemkin input file.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'CONTROL_1D'






