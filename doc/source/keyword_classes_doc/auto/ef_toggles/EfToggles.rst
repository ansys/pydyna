





:class:`EfToggles`
==================


.. py:class:: ef_toggles.EfToggles(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EF_TOGGLES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EfToggles

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~iprint1`
            - Get or set the Controls output of exchange fractions to the d3hsp file. In almost all situations this should be set to 0 because the “exchange factors” are written to the file exchfl.
          * - :py:attr:`~iprint2`
            - Get or set the Controls output of a list of lost photons to the d3hsp file. This is useful for debugging.
          * - :py:attr:`~iprint3`
            - Get or set the Controls output about the grid algorithm to the d3hsp file
          * - :py:attr:`~iprint4`
            - Get or set the Controls output about material information pertaining to exchange factors to the d3hsp file.
          * - :py:attr:`~idata`
            - Get or set the Controls execution
          * - :py:attr:`~itraces`
            - Get or set the ITRACES Controls output of photon trajectories.
          * - :py:attr:`~irstrt`
            - Get or set the IRESTART should be set either to 1 or 0.  If IRESTART is set to 1 then LS-DYNA restarts the exchange factor solver.  If IRESTART is set to 1 and a .crh file exists, the Monte Carlo solver will pick up where it left off prior to a crash.  If there is a .nij file but no .crh file, then LS-DYNA will recycle the results of the previous exchange factor running emitting more photons to increase accuracy


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

    from ef_toggles import EfToggles

Property detail
---------------

.. py:property:: iprint1
   :type: int


   
   Get or set the Controls output of exchange fractions to the d3hsp file. In almost all situations this should be set to 0 because the “exchange factors” are written to the file exchfl.
   EQ.0:   do not write exchange fractions
   EQ.1 : write exchange fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: iprint2
   :type: int


   
   Get or set the Controls output of a list of lost photons to the d3hsp file. This is useful for debugging.
   EQ.0:   do not write lost photon listEQ.1 : write lost photon list
















   ..
       !! processed by numpydoc !!

.. py:property:: iprint3
   :type: int


   
   Get or set the Controls output about the grid algorithm to the d3hsp file
   EQ.0:   do not write grid algorithm information
   EQ.1 : write grid algorithm information
















   ..
       !! processed by numpydoc !!

.. py:property:: iprint4
   :type: int


   
   Get or set the Controls output about material information pertaining to exchange factors to the d3hsp file.
   EQ.0:   do not write material information
   EQ.1 : write material information
















   ..
       !! processed by numpydoc !!

.. py:property:: idata
   :type: int


   
   Get or set the Controls execution
   EQ.0:   run proceeds
   EQ.1 : terminate after input parameter check
















   ..
       !! processed by numpydoc !!

.. py:property:: itraces
   :type: int


   
   Get or set the ITRACES Controls output of photon trajectories.
   EQ.0:   do not write trajectory information
   EQ.1 : write trajectory information.This file becomes large quickly and is only useful for debugging
















   ..
       !! processed by numpydoc !!

.. py:property:: irstrt
   :type: Optional[int]


   
   Get or set the IRESTART should be set either to 1 or 0.  If IRESTART is set to 1 then LS-DYNA restarts the exchange factor solver.  If IRESTART is set to 1 and a .crh file exists, the Monte Carlo solver will pick up where it left off prior to a crash.  If there is a .nij file but no .crh file, then LS-DYNA will recycle the results of the previous exchange factor running emitting more photons to increase accuracy
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EF'


.. py:attribute:: subkeyword
   :value: 'TOGGLES'






