





:class:`ChemistryControlInflator`
=================================


.. py:class:: chemistry_control_inflator.ChemistryControlInflator(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_INFLATOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControlInflator

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~model`
            - Get or set the Type of inflator model to compute.
          * - :py:attr:`~out_type`
            - Get or set the Selects the output file format that will be used in an airbag simulation.EQ.0:Screen output.
          * - :py:attr:`~truntim`
            - Get or set the Total run time.
          * - :py:attr:`~delt`
            - Get or set the Delta(t) to use in the model calculation.
          * - :py:attr:`~ptime`
            - Get or set the Time interval for output of time history data to FILE.
          * - :py:attr:`~file`
            - Get or set the Name of the ASCII file in which to write the time history data and other data output by the inflator simulation.
          * - :py:attr:`~density`
            - Get or set the Density of a condensed-phase species present in the inflator.
          * - :py:attr:`~species_name`
            - Get or set the Chemkin-compatible name of a condensed-phase species.


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

    from chemistry_control_inflator import ChemistryControlInflator

Property detail
---------------

.. py:property:: model
   :type: int


   
   Get or set the Type of inflator model to compute.
   EQ.1:Pyrotechnic model.
   EQ.2:Hybrid model with cold flow option in the gas chamber.
   EQ.3:Hybrid model with heat flow in the gas chamber.
















   ..
       !! processed by numpydoc !!

.. py:property:: out_type
   :type: int


   
   Get or set the Selects the output file format that will be used in an airbag simulation.EQ.0:Screen output.
   EQ.1:CESE compressible flow solver.
   EQ.2:ALE solver.
   EQ.3:CPM solver(with 2nd-order expansion of Cp)
   EQ.4:CPM solver(with 4th-order expansion of Cp)
















   ..
       !! processed by numpydoc !!

.. py:property:: truntim
   :type: Optional[float]


   
   Get or set the Total run time.
















   ..
       !! processed by numpydoc !!

.. py:property:: delt
   :type: Optional[float]


   
   Get or set the Delta(t) to use in the model calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptime
   :type: Optional[float]


   
   Get or set the Time interval for output of time history data to FILE.
















   ..
       !! processed by numpydoc !!

.. py:property:: file
   :type: Optional[str]


   
   Get or set the Name of the ASCII file in which to write the time history data and other data output by the inflator simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: density
   :type: Optional[str]


   
   Get or set the Density of a condensed-phase species present in the inflator.
















   ..
       !! processed by numpydoc !!

.. py:property:: species_name
   :type: Optional[str]


   
   Get or set the Chemkin-compatible name of a condensed-phase species.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CHEMISTRY'


.. py:attribute:: subkeyword
   :value: 'CONTROL_INFLATOR'






