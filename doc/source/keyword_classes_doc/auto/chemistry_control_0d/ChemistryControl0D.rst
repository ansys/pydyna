





:class:`ChemistryControl0D`
===========================


.. py:class:: chemistry_control_0d.ChemistryControl0D(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_0D keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControl0D

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identifier for this 0D computation.
          * - :py:attr:`~compid`
            - Get or set the Chemical composition identifier of composition to use.
          * - :py:attr:`~soltyp`
            - Get or set the Type of 0D calculation:
          * - :py:attr:`~plotdt`
            - Get or set the Error tolerance for the calculation.
          * - :py:attr:`~csp_sel`
            - Get or set the CSP solver option:
          * - :py:attr:`~dt`
            - Get or set the Initial time step.
          * - :py:attr:`~tlimit`
            - Get or set the Time limit for the simulation.
          * - :py:attr:`~tic`
            - Get or set the Initial temperature.
          * - :py:attr:`~pic`
            - Get or set the Initial pressure.
          * - :py:attr:`~ric`
            - Get or set the Initial density.
          * - :py:attr:`~eic`
            - Get or set the Initial internal energy.
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

    from chemistry_control_0d import ChemistryControl0D

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identifier for this 0D computation.
















   ..
       !! processed by numpydoc !!

.. py:property:: compid
   :type: Optional[int]


   
   Get or set the Chemical composition identifier of composition to use.
















   ..
       !! processed by numpydoc !!

.. py:property:: soltyp
   :type: int


   
   Get or set the Type of 0D calculation:
   EQ.1: Isochoric
   EQ.2: Isobaric
















   ..
       !! processed by numpydoc !!

.. py:property:: plotdt
   :type: float


   
   Get or set the Error tolerance for the calculation.
















   ..
       !! processed by numpydoc !!

.. py:property:: csp_sel
   :type: int


   
   Get or set the CSP solver option:
   EQ.0: Do not use the CSP solver, and ignore the AMPL and YCUT parameters (default).
   GT.0: Use the CSP solver, with the AMPL and YCUT parameters.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Initial time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: tlimit
   :type: Optional[float]


   
   Get or set the Time limit for the simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: tic
   :type: Optional[float]


   
   Get or set the Initial temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: pic
   :type: Optional[float]


   
   Get or set the Initial pressure.
















   ..
       !! processed by numpydoc !!

.. py:property:: ric
   :type: Optional[float]


   
   Get or set the Initial density.
















   ..
       !! processed by numpydoc !!

.. py:property:: eic
   :type: Optional[float]


   
   Get or set the Initial internal energy.
















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
   :value: 'CONTROL_0D'






