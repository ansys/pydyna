





:class:`FatigueElout`
=====================


.. py:class:: fatigue_elout.FatigueElout(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FATIGUE_ELOUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FatigueElout

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

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

    from fatigue_elout import FatigueElout

Property detail
---------------

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
   :value: 'ELOUT'






