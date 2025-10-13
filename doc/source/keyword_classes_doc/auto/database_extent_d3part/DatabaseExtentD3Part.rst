





:class:`DatabaseExtentD3Part`
=============================


.. py:class:: database_extent_d3part.DatabaseExtentD3Part(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_EXTENT_D3PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseExtentD3Part

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~neiph`
            - Get or set the Number of additional integration point history variables written to the
          * - :py:attr:`~neips`
            - Get or set the Number of additional integration point history variables written to the
          * - :py:attr:`~maxint`
            - Get or set the Number of shell integration points written to the binary database, see
          * - :py:attr:`~strflg`
            - Get or set the Set to 1 to dump strain tensors for solid, shell and thick shell elements
          * - :py:attr:`~sigflg`
            - Get or set the Flag for including stress tensor in the shell LS-DYNA database:
          * - :py:attr:`~epsflg`
            - Get or set the Flag for including the effective plastic strains in the shell LS-DYNA database:
          * - :py:attr:`~rltflg`
            - Get or set the Flag for including stress resultants in the shell LS-DYNA database:
          * - :py:attr:`~engflg`
            - Get or set the Flag for including internal energy density and thickness in the LS-DYNA database:
          * - :py:attr:`~ieverp`
            - Get or set the Every plot state for D3PART database is written to a separate file. This option will limit the database to 100 states:
          * - :py:attr:`~shge`
            - Get or set the Output shell hourglass energy density:
          * - :py:attr:`~stssz`
            - Get or set the Output shell element time step, mass or added mass:
          * - :py:attr:`~nintsld`
            - Get or set the Number of solid element integration points written to the LS-DYNA


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

    from database_extent_d3part import DatabaseExtentD3Part

Property detail
---------------

.. py:property:: neiph
   :type: int


   
   Get or set the Number of additional integration point history variables written to the
   binary database for solid elements. The integration point data is written
   in the same order that it is stored in memory-each material model has its
   own history variables that are stored. For user defined materials it is
   important to store the history data that is needed for plotting before the
   data which is not of interest.
















   ..
       !! processed by numpydoc !!

.. py:property:: neips
   :type: int


   
   Get or set the Number of additional integration point history variables written to the
   binary database for both shell and thick shell elements for each
   integration point, see NEIPH above.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxint
   :type: int


   
   Get or set the Number of shell integration points written to the binary database, see
   also *INTEGRATION_SHELL. If the default value of 3 is used then
   results are output for the outermost (top) and innermost (bottom)
   integration points together with results for the neutral axis. If MAXINT
   is set to 3 and the element has 1 integration point then all three results
   will be the same. If a value other than 3 is used then results for the first
   MAXINT integration points in the element will be output. Note: If the
   element has an even number of integration points and MAXINT is not
   set to 3 then you will not get mid-surface results. See Remarks below.
   If MAXINT is set to a negative number, MAXINT integration points are
   output for each in plane integration point location and no averaging is
   used. This can greatly increase the size of the binary databases
   D3PLOT, D3THDT, and D3PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: strflg
   :type: int


   
   Get or set the Set to 1 to dump strain tensors for solid, shell and thick shell elements
   for plotting by LS-PREPOST and ASCII file ELOUT. For shell and
   thick shell elements two tensors are written, one at the innermost and
   one at the outermost integration point. For solid elements a single strain
   tensor is written.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigflg
   :type: int


   
   Get or set the Flag for including stress tensor in the shell LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: epsflg
   :type: int


   
   Get or set the Flag for including the effective plastic strains in the shell LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: rltflg
   :type: int


   
   Get or set the Flag for including stress resultants in the shell LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: engflg
   :type: int


   
   Get or set the Flag for including internal energy density and thickness in the LS-DYNA database:
   EQ.1: include (default),
   EQ.2: exclude.
















   ..
       !! processed by numpydoc !!

.. py:property:: ieverp
   :type: int


   
   Get or set the Every plot state for D3PART database is written to a separate file. This option will limit the database to 100 states:
   EQ.0: more than one state can be on each plotfile,
   EQ.1: one state only on each plotfile.
















   ..
       !! processed by numpydoc !!

.. py:property:: shge
   :type: int


   
   Get or set the Output shell hourglass energy density:
   EQ.1: off (default), no hourglass energy written,
   EQ.2: on.
















   ..
       !! processed by numpydoc !!

.. py:property:: stssz
   :type: int


   
   Get or set the Output shell element time step, mass or added mass:
   EQ.1: off (default),
   EQ.2: out time step size,
   EQ.3: output mass, added mass, or time step size.
   (See Remark 3 in user's manual).
















   ..
       !! processed by numpydoc !!

.. py:property:: nintsld
   :type: int


   
   Get or set the Number of solid element integration points written to the LS-DYNA
   database. The default value is 1. For solids with multiple integration
   points NINTSLD may be set to 8. Currently, no other values for
   NINTSLD are allowed. For solids with multiple integration points, an
   average value is output if NINTSLD is set to 1.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'EXTENT_D3PART'






