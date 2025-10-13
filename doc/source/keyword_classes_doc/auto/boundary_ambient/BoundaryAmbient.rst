





:class:`BoundaryAmbient`
========================


.. py:class:: boundary_ambient.BoundaryAmbient(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_AMBIENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAmbient

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~setid`
            - Get or set the The ambient element set ID for which the thermodynamic state is being defined. The element set can be *SET_SOLID for a 3D ALE model, *SET_SHELL for a 2D ALE model or *SET_BEAM for a 1D ALE model.
          * - :py:attr:`~mmg`
            - Get or set the ALE multi-material group ID.In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID
          * - :py:attr:`~ambtyp`
            - Get or set the Ambient element type:
          * - :py:attr:`~sidr`
            - Get or set the Flag controlling the use of this keyword during dynamic relaxation.
          * - :py:attr:`~lcid1`
            - Get or set the A load curve ID for internal energy per unit reference volume (see Remark 4 and the *EOS section for details). If *EOS_‌IDEAL_‌GAS is being used, this ID refers to a temperature load curve ID
          * - :py:attr:`~lcid2`
            - Get or set the Load curve ID for relative volume, v_r=(v⁄v_0 =ρ_0⁄ρ).  (See Remark 3 and the *EOS section for details).


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

    from boundary_ambient import BoundaryAmbient

Property detail
---------------

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the The ambient element set ID for which the thermodynamic state is being defined. The element set can be *SET_SOLID for a 3D ALE model, *SET_SHELL for a 2D ALE model or *SET_BEAM for a 1D ALE model.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmg
   :type: Optional[int]


   
   Get or set the ALE multi-material group ID.In case of S-ALE, AMMG name (AMMGNM) could be also used in place of AMMGID
















   ..
       !! processed by numpydoc !!

.. py:property:: ambtyp
   :type: Optional[int]


   
   Get or set the Ambient element type:
   EQ.4:   Pressure inflow/outflow (see Remarks 1 and 2)
   EQ.5:   Receptor for blast load (See *LOAD_BLAST_ENHANCED)
















   ..
       !! processed by numpydoc !!

.. py:property:: sidr
   :type: int


   
   Get or set the Flag controlling the use of this keyword during dynamic relaxation.
   EQ.0:   the keyword is applied in normal analysis phase only,
   EQ.1:   the keyword is applied in dynamic relaxation phase but not the normal analysis phase,
   EQ.2:   the keyword is applied in both dynamic relaxation phase and normal analysis phase.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid1
   :type: Optional[int]


   
   Get or set the A load curve ID for internal energy per unit reference volume (see Remark 4 and the *EOS section for details). If *EOS_‌IDEAL_‌GAS is being used, this ID refers to a temperature load curve ID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid2
   :type: Optional[int]


   
   Get or set the Load curve ID for relative volume, v_r=(v⁄v_0 =ρ_0⁄ρ).  (See Remark 3 and the *EOS section for details).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'AMBIENT'






