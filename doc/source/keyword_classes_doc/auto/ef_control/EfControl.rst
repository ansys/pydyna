





:class:`EfControl`
==================


.. py:class:: ef_control.EfControl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EF_CONTROL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EfControl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nphton`
            - Get or set the The base number of photons emitted per band per surface per convergence loop.  Note that NPHT from *BOUNDARY_‌RADIATION_‌SET_‌EF_‌CALCULATE also effects the number of photons emitted per surface per band per convergence loop
          * - :py:attr:`~nrefs`
            - Get or set the The maximum number of reflections allowed per photon before LS-DYNA issues a warning
          * - :py:attr:`~nwarns`
            - Get or set the The maximum number of warnings allowed per surface before the run is aborted
          * - :py:attr:`~nlost`
            - Get or set the The maximum number of lost photons allowed per surface.  Round off error often causes the loss of photons, so this number ought not to be set too small (usually the default is reasonable).
          * - :py:attr:`~nloops`
            - Get or set the This specifies the maximum number of convergence loops.  If the relative error obtained upon the completion of a run is not within the specified tolerances, LS-DYNA will rerun the model combining the results of all previous runs together with the results of the present run to obtain a more accurate result.  LS-DYNA will rerun the problem NLOOPS times to achieve error margins within the specified tolerances.  If the desired level of convergence is not obtained within NLOOPS iterations LS-DYNA error terminates.
          * - :py:attr:`~errodef`
            - Get or set the Specifies that tolerance for convergence of the surface exchange fractions.  This may be overridden on a surface by surface basis with the ERRMAX setting. (see *BOUNDARY_‌RADIATION_‌SET_‌EF_‌CALCULATE)
          * - :py:attr:`~inseed`
            - Get or set the Tells LS-DYNA how to obtain an initial seed for the Monte Carlo random number generator.


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

    from ef_control import EfControl

Property detail
---------------

.. py:property:: nphton
   :type: Optional[int]


   
   Get or set the The base number of photons emitted per band per surface per convergence loop.  Note that NPHT from *BOUNDARY_‌RADIATION_‌SET_‌EF_‌CALCULATE also effects the number of photons emitted per surface per band per convergence loop
















   ..
       !! processed by numpydoc !!

.. py:property:: nrefs
   :type: Optional[int]


   
   Get or set the The maximum number of reflections allowed per photon before LS-DYNA issues a warning
















   ..
       !! processed by numpydoc !!

.. py:property:: nwarns
   :type: Optional[int]


   
   Get or set the The maximum number of warnings allowed per surface before the run is aborted
















   ..
       !! processed by numpydoc !!

.. py:property:: nlost
   :type: Optional[int]


   
   Get or set the The maximum number of lost photons allowed per surface.  Round off error often causes the loss of photons, so this number ought not to be set too small (usually the default is reasonable).
















   ..
       !! processed by numpydoc !!

.. py:property:: nloops
   :type: Optional[int]


   
   Get or set the This specifies the maximum number of convergence loops.  If the relative error obtained upon the completion of a run is not within the specified tolerances, LS-DYNA will rerun the model combining the results of all previous runs together with the results of the present run to obtain a more accurate result.  LS-DYNA will rerun the problem NLOOPS times to achieve error margins within the specified tolerances.  If the desired level of convergence is not obtained within NLOOPS iterations LS-DYNA error terminates.
















   ..
       !! processed by numpydoc !!

.. py:property:: errodef
   :type: Optional[float]


   
   Get or set the Specifies that tolerance for convergence of the surface exchange fractions.  This may be overridden on a surface by surface basis with the ERRMAX setting. (see *BOUNDARY_‌RADIATION_‌SET_‌EF_‌CALCULATE)
















   ..
       !! processed by numpydoc !!

.. py:property:: inseed
   :type: Optional[int]


   
   Get or set the Tells LS-DYNA how to obtain an initial seed for the Monte Carlo random number generator.
   EQ.0:   use date and time.
   GT.0 : use INSEED as seed.
   LT.0 : use a default seed.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EF'


.. py:attribute:: subkeyword
   :value: 'CONTROL'






