





:class:`AleSwitchMmg`
=====================


.. py:class:: ale_switch_mmg.AleSwitchMmg(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_SWITCH_MMG keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleSwitchMmg

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fr_mmg`
            - Get or set the This is the AMMG-SID before the switch. The AMMG-SID
          * - :py:attr:`~to_mmg`
            - Get or set the This is the AMMG-SID after the switch. The AMMG-SID
          * - :py:attr:`~idfunc`
            - Get or set the ID of a *DEFINE_FUNCTION function. This function determines
          * - :py:attr:`~idsegset`
            - Get or set the ID of *SEGMENT_SET that is used to pass geometric properties to
          * - :py:attr:`~idsldset`
            - Get or set the The ID of a *SOLID_SET specifying which elements are affected
          * - :py:attr:`~ncycseg`
            - Get or set the Number of cycles between each update of the segment centers
          * - :py:attr:`~ncycsld`
            - Get or set the Number of cycles between each update of the ALE element
          * - :py:attr:`~var1`
            - Get or set the Variable rank in the following list (See Remark 2):
          * - :py:attr:`~var2`
            - Get or set the Variable rank in the following list (See Remark 2):
          * - :py:attr:`~var3`
            - Get or set the Variable rank in the following list (See Remark 2):
          * - :py:attr:`~var4`
            - Get or set the Variable rank in the following list (See Remark 2):
          * - :py:attr:`~var5`
            - Get or set the Variable rank in the following list (See Remark 2):
          * - :py:attr:`~var6`
            - Get or set the Variable rank in the following list (See Remark 2):
          * - :py:attr:`~var7`
            - Get or set the Variable rank in the following list (See Remark 2):
          * - :py:attr:`~var8`
            - Get or set the Variable rank in the following list (See Remark 2):


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

    from ale_switch_mmg import AleSwitchMmg

Property detail
---------------

.. py:property:: fr_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMG-SID before the switch. The AMMG-SID
   corresponds to the SID defined on a *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.
   This SID refers to one or more AMMGs.
















   ..
       !! processed by numpydoc !!

.. py:property:: to_mmg
   :type: Optional[int]


   
   Get or set the This is the AMMG-SID after the switch. The AMMG-SID
   corresponds to the SID defined on a *SET_MULTI-MATERIAL_GROUP_LIST (SMMGL) card.
   This SID refers to one or more AMMGs.
















   ..
       !! processed by numpydoc !!

.. py:property:: idfunc
   :type: Optional[int]


   
   Get or set the ID of a *DEFINE_FUNCTION function. This function determines
   the material fraction to be switched.
















   ..
       !! processed by numpydoc !!

.. py:property:: idsegset
   :type: int


   
   Get or set the ID of *SEGMENT_SET that is used to pass geometric properties to
   the function specified by IDFUNC. This field is optional.
   The segment center positions and normal vectors are computed.
   For each ALE element, this data is passed to the function
   IDFUNC for the segment the closest to the element center.
















   ..
       !! processed by numpydoc !!

.. py:property:: idsldset
   :type: int


   
   Get or set the The ID of a *SOLID_SET specifying which elements are affected
   by this particular instance of the *ALE_SWITCH_MMG keyword.
   This field is optional. If undefined, *ALE_SWITCH_MMG affects
   all ALE elements. The element centers are computed and can be
   used as variables in the function IDFUNC.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycseg
   :type: int


   
   Get or set the Number of cycles between each update of the segment centers
   and normal vectors (if a segment set is defined). For each update,
   a bucket sort is applied to find the closest segment to each ALE
   element. If the segment nodes are fully constrained, the segment
   centers and normal vectors are computed only one time.
















   ..
       !! processed by numpydoc !!

.. py:property:: ncycsld
   :type: int


   
   Get or set the Number of cycles between each update of the ALE element
   centers. For each update, a bucket sort is applied to find the
   closest segment to each ALE element. If the element nodes does
   not move (as with AFAC = -1 in *CONTROL_ALE) the element
   centers are computed exactly once.
















   ..
       !! processed by numpydoc !!

.. py:property:: var1
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: var2
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: var3
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: var4
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: var5
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: var6
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: var7
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!

.. py:property:: var8
   :type: int


   
   Get or set the Variable rank in the following list (See Remark 2):
   EQ.0: See Remark 3
   EQ.1: ....-stress for FR_MMG
   EQ.2: ....-stress for FR_MMG
   EQ.3: ....-stress for FR_MMG
   EQ.4: ....-stress for FR_MMG
   EQ.5: ....-stress for FR_MMG
   EQ.6: ....-stress for FR_MMG
   EQ.7: plastic strain for FR_MMG
   EQ.8: internal energy for FR_MMG
   EQ.9: bulk viscosity for FR_MMG
   EQ.10: volume from previous cycle for FR_MMG
   GE.11 and LE.20: other auxiliary variables for FR_MMG
   GE.21 and LE.40: auxiliary variables for TO_MMG (xx-stress, ...)
   EQ.41: mass for FR_MMG
   EQ.42: mass for TO_MMG
   EQ.43: volume fraction for FR_MMG
   EQ.44: volume fraction for TO_MMG
   EQ.45: material volume for FR_MMG
   EQ.46: material volume for TO_MMG
   EQ.47: time
   EQ.48: cycle
   EQ.49: x-position of the ALE element center
   EQ.50: y-position of the ALE element center
   EQ.51: z-position of the ALE element center
   EQ.52: x-position of the segment center
   EQ.53: y-position of the segment center
   EQ.54: 𝑧-position of the segment center
   EQ.55: x-component of the segment normal
   EQ.56: y-component of the segment normal
   EQ.57: z-component of the segment normal
   GE.58 and LE.65: x-positions of the ALE nodes
   GE.66 and LE.69: x-positions of the segment nodes
   GE.70 and LE.77: y-positions of the ALE nodes
   GE.79 and LE.81: y-positions of the segment nodes
   GE.83 and LE.89: z-positions of the ALE nodes
   GE.90 and LE.93: z-positions of the segment nodes
   GE.94 and LE.101: x-velocities of the ALE nodes
   GE.102 and LE.105: ..-velocities of the segment nodes
   GE.106 and LE.113: ..-velocities of the ALE nodes
   GE.114 and LE.117: ..-velocities of the segment nodes
   GE.118 and LE.125: ..-velocities of the ALE nodes
   GE.126 and LE.129: ..-velocities of the segment nodes
   GE.130 and LE.137: x-accelerations of the ALE nodes
   GE.138 and LE.141: x-accelerations of the segment nodes
   GE.142 and LE.149: y-accelerations of the ALE nodes
   GE.150 and LE.153: y-accelerations of the segment nodes
   GE.154 and LE.161: z-accelerations of the ALE nodes
   GE.162 and LE.165: z-accelerations of the segment nodes
   GE.166 and LE.173: masses of the ALE nodes
   GE.174 and LE.177: masses of the segment nodes
   EQ.178: rank of the variable updated by the function (See Remark 4)
   EQ.179: rank of the multi-material group in the set
   EQ.180: time step.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'SWITCH_MMG'






