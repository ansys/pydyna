





:class:`ChemistryControlCsp`
============================


.. py:class:: chemistry_control_csp.ChemistryControlCsp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CHEMISTRY_CONTROL_CSP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ChemistryControlCsp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identifier for this computational singular perturbation solver.
          * - :py:attr:`~ierropt`
            - Get or set the Selector:
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

    from chemistry_control_csp import ChemistryControlCsp

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identifier for this computational singular perturbation solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: ierropt
   :type: Optional[int]


   
   Get or set the Selector:
   EQ.0: AMPL and YCUT values for all chemical species are required.
   EQ.1: One CSP Parameter Card should be provided, and it will be used for all species.
















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
   :value: 'CONTROL_CSP'






