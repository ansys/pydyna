





:class:`AleBurnSwitchMmg`
=========================


.. py:class:: ale_burn_switch_mmg.AleBurnSwitchMmg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_BURN_SWITCH_MMG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleBurnSwitchMmg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mmgfr`
            - Get or set the ALE multi-material-group (explosive) before the switch.
          * - :py:attr:`~mmgto`
            - Get or set the ALE multi-material-group (explosion product) after the switch.
          * - :py:attr:`~nvarline`
            - Get or set the Number of lines with arguments in the functions REACT, IGNI and IGNIV.
          * - :py:attr:`~react`
            - Get or set the ID of the *DEFINE_FUNCTION function controlling the reaction rate.
          * - :py:attr:`~igni`
            - Get or set the ID of the *DEFINE_FUNCTION function controlling the conditions of ignition.
          * - :py:attr:`~igniv`
            - Get or set the ID of the *DEFINE_FUNCTION function computing the ignition front speed.  See Remark 1.
          * - :py:attr:`~ignivf`
            - Get or set the Flag that activates computing the ignition front as a material interface between MMGFR and MMGTO.
          * - :py:attr:`~var`
            - Get or set the Variable rank in the following list (see Remark 3):
          * - :py:attr:`~par`
            - Get or set the User define routines parameters.


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

    from ale_burn_switch_mmg import AleBurnSwitchMmg

Property detail
---------------

.. py:property:: mmgfr
   :type: Optional[int]


   
   Get or set the ALE multi-material-group (explosive) before the switch.
















   ..
       !! processed by numpydoc !!

.. py:property:: mmgto
   :type: Optional[int]


   
   Get or set the ALE multi-material-group (explosion product) after the switch.
















   ..
       !! processed by numpydoc !!

.. py:property:: nvarline
   :type: int


   
   Get or set the Number of lines with arguments in the functions REACT, IGNI and IGNIV.
















   ..
       !! processed by numpydoc !!

.. py:property:: react
   :type: int


   
   Get or set the ID of the *DEFINE_FUNCTION function controlling the reaction rate.
   This function determines the explosive volume fraction to be switched.
















   ..
       !! processed by numpydoc !!

.. py:property:: igni
   :type: int


   
   Get or set the ID of the *DEFINE_FUNCTION function controlling the conditions of ignition.
















   ..
       !! processed by numpydoc !!

.. py:property:: igniv
   :type: int


   
   Get or set the ID of the *DEFINE_FUNCTION function computing the ignition front speed.  See Remark 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: ignivf
   :type: int


   
   Get or set the Flag that activates computing the ignition front as a material interface between MMGFR and MMGTO.
   This flag will be automatically activated if both IGNI and IGNIVF are undefined (see Remark 2).
   EQ.0:   not activated
   EQ.1:   activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: var
   :type: int


   
   Get or set the Variable rank in the following list (see Remark 3):
   EQ.1:   -stress for MMGFR
   EQ.2:   -stress for MMGFR
   EQ.3:   -stress for MMGFR
   EQ.4:   -stress for MMGFR
   EQ.5:   -stress for MMGFR
   EQ.6:   -stress for MMGFR
   EQ.7:   plastic strain for MMGFR
   EQ.8:   internal energy for MMGFR
   EQ.9:   bulk viscosity for MMGFR
   EQ.10:  volume from previous cycle for MMGFR
   GE.11 and LE.20:        other auxiliary variables for MMGFR
   GE.21 and LE.40:        auxiliary variables for MMGTO (-stress, …)
   EQ.41:  mass for MMGFR
   EQ.42:  mass for MMGTO
   EQ.43:  volume fraction for MMGFR
   EQ.44:  volume fraction for MMGTO
   EQ.45:  material volume for MMGFR
   EQ.46:  material volume for MMGTO
   EQ.47:  time step
   EQ.48:  time
   EQ.49:  cycle
   GE.50 and LE.57:        -positions of the ALE nodes
   GE.58 and LE.65:        -positions of the ALE nodes
   GE.66 and LE.73:        -positions of the ALE nodes
   GE.74 and LE.81:        -velocities of the ALE nodes
   GE.82 and LE.89:        -velocities of the ALE nodes
   GE.90 and LE.97:        -velocities of the ALE nodes
   GE.98 and LE.105:       -accelerations of the ALE nodes
   GE.106 and LE.113:      -accelerations of the ALE nodes
   GE.114 and LE.121:      -accelerations of the ALE nodes
   GE.122 and LE.129:      masses of the ALE nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: par
   :type: float


   
   Get or set the User define routines parameters.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'BURN_SWITCH_MMG'






