





:class:`IncludeStampedSetMatrixInverse`
=======================================


.. py:class:: include_stamped_set_matrix_inverse.IncludeStampedSetMatrixInverse(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_STAMPED_SET_MATRIX_INVERSE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeStampedSetMatrixInverse

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the File name of file to be included in this keyword file.
          * - :py:attr:`~psid`
            - Get or set the Part Set ID of crash part for remapping.
          * - :py:attr:`~thick`
            - Get or set the Thickness remap:
          * - :py:attr:`~pstrn`
            - Get or set the Plastic strain remap:
          * - :py:attr:`~strain`
            - Get or set the Strain remap:
          * - :py:attr:`~stress`
            - Get or set the Stress tensor remap:
          * - :py:attr:`~incout`
            - Get or set the Save mapped data:
          * - :py:attr:`~rmax`
            - Get or set the Search radius.  LS-DYNA remaps history variables from the mesh of the stamped part to the mesh of the crash part with a spatial tolerance of RMAX.  If an element in the crash part lies within RMAX of the stamped part, data will be mapped to that element.  If set less than 0.001, RMAX automatically assumes the default value of 20.
          * - :py:attr:`~r11`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~r12`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~r13`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~xp`
            - Get or set the Translational distance
          * - :py:attr:`~r21`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~r22`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~r23`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~yp`
            - Get or set the Translational distance
          * - :py:attr:`~r31`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~r32`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~r33`
            - Get or set the Components of the transformation matrix
          * - :py:attr:`~zp`
            - Get or set the Translational distance
          * - :py:attr:`~isym`
            - Get or set the Symmetric switch
          * - :py:attr:`~iafter`
            - Get or set the Mirroring sequence switch
          * - :py:attr:`~percele`
            - Get or set the Percentage of elements that should be mapped for the simulation to proceed (default = 0); otherwise an error termination occurs. See Remark 6
          * - :py:attr:`~iortho`
            - Get or set the Location of the material direction cosine in the array of history variables of an orthotropic material
          * - :py:attr:`~isrocut`
            - Get or set the Optional output of stamped part after transformation(s)
          * - :py:attr:`~x01`
            - Get or set the First point in the symmetric plane (required if ISYM.NE.0)
          * - :py:attr:`~y01`
            - Get or set the First point in the symmetric plane (required if ISYM.NE.0)
          * - :py:attr:`~z01`
            - Get or set the First point in the symmetric plane (required if ISYM.NE.0)
          * - :py:attr:`~x02`
            - Get or set the Second point in the symmetric plane
          * - :py:attr:`~y02`
            - Get or set the Second point in the symmetric plane
          * - :py:attr:`~z02`
            - Get or set the Second point in the symmetric plane
          * - :py:attr:`~x03`
            - Get or set the Third point in the symmetric plane
          * - :py:attr:`~y03`
            - Get or set the Third point in the symmetric plane
          * - :py:attr:`~z03`
            - Get or set the Third point in the symmetric plane


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

    from include_stamped_set_matrix_inverse import IncludeStampedSetMatrixInverse

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the File name of file to be included in this keyword file.
   Maximum 80 charcters. If the STAMPED_PART option is active, this is the DYNAIN file containing the results from metal stamping.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part Set ID of crash part for remapping.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: int


   
   Get or set the Thickness remap:
   EQ.0: map thickness
   EQ.1: do not map thickness
   EQ.2:   Average value inside a circle defined by RMAX
















   ..
       !! processed by numpydoc !!

.. py:property:: pstrn
   :type: int


   
   Get or set the Plastic strain remap:
   EQ.0: map plastic strain
   EQ.1: do not plastic strain
   EQ.2:   Average value inside a circle defined by RMAX
















   ..
       !! processed by numpydoc !!

.. py:property:: strain
   :type: int


   
   Get or set the Strain remap:
   EQ.0: map strains
   EQ.1: do not map strains
















   ..
       !! processed by numpydoc !!

.. py:property:: stress
   :type: int


   
   Get or set the Stress tensor remap:
   EQ.0: map stress tensorand history variables
   EQ.1:do not map stress tensor. only history varibales
   EQ.2:   Do not map stress tensor or history variables
   EQ. - 1:        Map stress tensor in an internal large format(binary files)
   EQ. - 3 : Do not map stress tensor in an internal large format, only history variables(binary files)
















   ..
       !! processed by numpydoc !!

.. py:property:: incout
   :type: int


   
   Get or set the Save mapped data:
   EQ.1:   Save the mapped data for the part / part set(PID) to a file called dyna.inc.This option is useful for when the mapped data may be required in a future simulation.
   EQ.2 : Save the mapped data for the specified part or part set(PID) to a file called dynain_‌xx(xx is the part or part set ID).
   EQ.3 : Save the mapped data for the specified part or part set(PID) to a file called nastran_‌xx(in nastran format).xx is the part or part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmax
   :type: float


   
   Get or set the Search radius.  LS-DYNA remaps history variables from the mesh of the stamped part to the mesh of the crash part with a spatial tolerance of RMAX.  If an element in the crash part lies within RMAX of the stamped part, data will be mapped to that element.  If set less than 0.001, RMAX automatically assumes the default value of 20.
















   ..
       !! processed by numpydoc !!

.. py:property:: r11
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: r12
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: r13
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the Translational distance
















   ..
       !! processed by numpydoc !!

.. py:property:: r21
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: r22
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: r23
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the Translational distance
















   ..
       !! processed by numpydoc !!

.. py:property:: r31
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: r32
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: r33
   :type: Optional[float]


   
   Get or set the Components of the transformation matrix
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the Translational distance
















   ..
       !! processed by numpydoc !!

.. py:property:: isym
   :type: int


   
   Get or set the Symmetric switch
   EQ.0:no symetric mapping
   EQ.1: yz plane symmetric mapping
   EQ.2: zx plane symmetric mapping
   EQ.3: zx and yz planes symmetric mapping
   EQ.4: user defined symmetric plane mapping
















   ..
       !! processed by numpydoc !!

.. py:property:: iafter
   :type: int


   
   Get or set the Mirroring sequence switch
   EQ.0: generate a symmetric part before transformation
   EQ.1: generate a symmetric part after transformation
















   ..
       !! processed by numpydoc !!

.. py:property:: percele
   :type: Optional[float]


   
   Get or set the Percentage of elements that should be mapped for the simulation to proceed (default = 0); otherwise an error termination occurs. See Remark 6
















   ..
       !! processed by numpydoc !!

.. py:property:: iortho
   :type: Optional[int]


   
   Get or set the Location of the material direction cosine in the array of history variables of an orthotropic material
















   ..
       !! processed by numpydoc !!

.. py:property:: isrocut
   :type: Optional[int]


   
   Get or set the Optional output of stamped part after transformation(s)
   EQ.0:   No output is written.
   NE.0 : Keyword output file “srcmsh_‌<ISRCOUT>” is created
















   ..
       !! processed by numpydoc !!

.. py:property:: x01
   :type: Optional[float]


   
   Get or set the First point in the symmetric plane (required if ISYM.NE.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: y01
   :type: Optional[float]


   
   Get or set the First point in the symmetric plane (required if ISYM.NE.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: z01
   :type: Optional[float]


   
   Get or set the First point in the symmetric plane (required if ISYM.NE.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: x02
   :type: Optional[float]


   
   Get or set the Second point in the symmetric plane
















   ..
       !! processed by numpydoc !!

.. py:property:: y02
   :type: Optional[float]


   
   Get or set the Second point in the symmetric plane
















   ..
       !! processed by numpydoc !!

.. py:property:: z02
   :type: Optional[float]


   
   Get or set the Second point in the symmetric plane
















   ..
       !! processed by numpydoc !!

.. py:property:: x03
   :type: Optional[float]


   
   Get or set the Third point in the symmetric plane
















   ..
       !! processed by numpydoc !!

.. py:property:: y03
   :type: Optional[float]


   
   Get or set the Third point in the symmetric plane
















   ..
       !! processed by numpydoc !!

.. py:property:: z03
   :type: Optional[float]


   
   Get or set the Third point in the symmetric plane
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'STAMPED_SET_MATRIX_INVERSE'






