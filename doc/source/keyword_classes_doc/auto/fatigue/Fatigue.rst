





:class:`Fatigue`
================


.. py:class:: fatigue.Fatigue(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FATIGUE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Fatigue

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID, Part Set ID, or Element (solid, shell, beam, thick shell) Set ID.
          * - :py:attr:`~ptype`
            - Get or set the Type of PID:
          * - :py:attr:`~dt`
            - Get or set the Time step for saving the stress/strain data in transient analysis
          * - :py:attr:`~strsn`
            - Get or set the Type of fatigue analysis variable:
          * - :py:attr:`~index`
            - Get or set the Stress/strain index for performing fatigue analysis:
          * - :py:attr:`~restrt`
            - Get or set the Restart options. This flag is used to save an LS-DYNA transient
          * - :py:attr:`~texpos`
            - Get or set the Exposure time. If this is 0, the exposure time is the same as ENDTIM in *CONTROL_TERMINATION.
          * - :py:attr:`~dmgmin`
            - Get or set the Minimum fatigue damage ratio for parts undergoing fatigue analysis:
          * - :py:attr:`~filename`
            - Get or set the Time step for saving the stress/strain data in transient analysis


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

    from fatigue import Fatigue

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, Part Set ID, or Element (solid, shell, beam, thick shell) Set ID.
   EQ.0: Fatigue analysis is performed on the whole structure
















   ..
       !! processed by numpydoc !!

.. py:property:: ptype
   :type: int


   
   Get or set the Type of PID:
   EQ.0: Part (default)
   EQ.1: Part set
   EQ.2: SET_SOLID
   EQ.3: SET_BEAM
   EQ.4: SET_SHELL
   EQ.5: SET_TSHELL
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Time step for saving the stress/strain data in transient analysis
















   ..
       !! processed by numpydoc !!

.. py:property:: strsn
   :type: int


   
   Get or set the Type of fatigue analysis variable:
   EQ.0: Stress (default)
   EQ.1: Strain
















   ..
       !! processed by numpydoc !!

.. py:property:: index
   :type: int


   
   Get or set the Stress/strain index for performing fatigue analysis:
   EQ.0: Von-Mises stress/strain
   EQ.1: Maximum principal stress/strain
   EQ.2: Maximum shear stress/strain
















   ..
       !! processed by numpydoc !!

.. py:property:: restrt
   :type: int


   
   Get or set the Restart options. This flag is used to save an LS-DYNA transient
   analysis if the binary database for stress/strain time history data
   has been created in last runs. See Remark 3.
   EQ.0: initial run
   EQ.1: restart with existing stress/strain binary database
















   ..
       !! processed by numpydoc !!

.. py:property:: texpos
   :type: float


   
   Get or set the Exposure time. If this is 0, the exposure time is the same as ENDTIM in *CONTROL_TERMINATION.
















   ..
       !! processed by numpydoc !!

.. py:property:: dmgmin
   :type: float


   
   Get or set the Minimum fatigue damage ratio for parts undergoing fatigue analysis:
   EQ.0:   no change on computed fatigue damage ratio
   LT.0 : for each part, the minimum fatigue damage ratio dumped to D3FTG is | DMGMIN | x the computed nonzero minimum fatigue damage ratio computed on the current part.
   GT.0 : for each part, the minimum fatigue damage ratio dumped to D3FTG is DMGMIN.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the Time step for saving the stress/strain data in transient analysis
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FATIGUE'


.. py:attribute:: subkeyword
   :value: 'FATIGUE'






