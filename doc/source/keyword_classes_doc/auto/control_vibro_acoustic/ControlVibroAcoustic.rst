





:class:`ControlVibroAcoustic`
=============================


.. py:class:: control_vibro_acoustic.ControlVibroAcoustic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_VIBRO_ACOUSTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlVibroAcoustic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~vaflag`
            - Get or set the Loading type:
          * - :py:attr:`~vaprld`
            - Get or set the Flag for including preload:
          * - :py:attr:`~vastrs`
            - Get or set the Flag for including stress analysis:
          * - :py:attr:`~vapsd`
            - Get or set the Flag for PSD output:
          * - :py:attr:`~varms`
            - Get or set the Flag for RMS output:
          * - :py:attr:`~vaplot`
            - Get or set the Flag for PSD broadband plots:
          * - :py:attr:`~ipanelu`
            - Get or set the Number of strips in U direction
          * - :py:attr:`~ipanelv`
            - Get or set the Number of strips in V direction
          * - :py:attr:`~restart`
            - Get or set the EQ.0: No restart will be requested. All intermediate output is deleted.
          * - :py:attr:`~nmodstr`
            - Get or set the Number of modes in modal stresses/strains output.


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

    from control_vibro_acoustic import ControlVibroAcoustic

Property detail
---------------

.. py:property:: vaflag
   :type: int


   
   Get or set the Loading type:
   EQ.0: No vibro-acoustic structural analysis.
   EQ.1: Base acceleration.
   EQ.2: Random pressure.
   EQ.3: Plane wave.
   EQ.4: Shock.
   EQ.5: Progressive wave.
   EQ.6: Reverberant wave.
   EQ.7: Turbulent boundary layer.
   EQ.8: Nodal force.
   EQ.9: Modal stresses/strains output only.
















   ..
       !! processed by numpydoc !!

.. py:property:: vaprld
   :type: int


   
   Get or set the Flag for including preload:
   EQ.0: No preload.
   EQ.1: Thermal preload due to temperature difference from the neutral temperature.
   EQ.2: Mechanical preload due to static pressure.
   EQ.3: Mechanical preload due to concentrated nodal force
















   ..
       !! processed by numpydoc !!

.. py:property:: vastrs
   :type: int


   
   Get or set the Flag for including stress analysis:
   EQ.0: No stress analysis, only displacement analysis is requested.
   EQ.1: Both stress and displacement analyses are requested.
















   ..
       !! processed by numpydoc !!

.. py:property:: vapsd
   :type: int


   
   Get or set the Flag for PSD output:
   EQ.0: No PSD output is requested.
   EQ.1: PSD output is requested
















   ..
       !! processed by numpydoc !!

.. py:property:: varms
   :type: int


   
   Get or set the Flag for RMS output:
   EQ.0: No RMS output is requested.
   EQ.1: RMS output is requested.
















   ..
       !! processed by numpydoc !!

.. py:property:: vaplot
   :type: int


   
   Get or set the Flag for PSD broadband plots:
   EQ.0: No PSD broadband plot is requested.
   EQ.1: PSD broadband plots are requested.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipanelu
   :type: Optional[int]


   
   Get or set the Number of strips in U direction
















   ..
       !! processed by numpydoc !!

.. py:property:: ipanelv
   :type: Optional[int]


   
   Get or set the Number of strips in V direction
















   ..
       !! processed by numpydoc !!

.. py:property:: restart
   :type: int


   
   Get or set the EQ.0: No restart will be requested. All intermediate output is deleted.
   EQ.1: Intermediate output is retained for restart.
   EQ.2: Restart based on intermediate output in last run. All intermediate output is deleted after the current run.
   EQ.3: Restart based on intermediate output in last run. All intermediate output is retained for next restart run..
















   ..
       !! processed by numpydoc !!

.. py:property:: nmodstr
   :type: Optional[int]


   
   Get or set the Number of modes in modal stresses/strains output.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'VIBRO_ACOUSTIC'






