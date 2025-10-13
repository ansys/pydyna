





:class:`IcfdControlOutput`
==========================


.. py:class:: icfd_control_output.IcfdControlOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~msgl`
            - Get or set the Message level.
          * - :py:attr:`~outl`
            - Get or set the Output the fluid results in other file formats apart from d3plot.
          * - :py:attr:`~dtout`
            - Get or set the Time interval to print the output when OUTL is different than 0.
          * - :py:attr:`~lsppout`
            - Get or set the EQ.1:    outputs a file with the automatically created fluid volume mesh in  a format compatible for LSPP at each remesh. Also outputs the fluid volume mesh in a format compatible with a subsequent ICFD analysis.
          * - :py:attr:`~itout`
            - Get or set the Iteration interval to print the output, including the d3plot files when the steady state slover is selected
          * - :py:attr:`~pitout`
            - Get or set the Pressure iteration limit output. If the number of pressure iterations in the fractional step solve goes above PITOUT, an extra d3plot will be dumped. This is mainly a debugging feature which can help the user identify problematic areas in the model which often precede a divergence


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

    from icfd_control_output import IcfdControlOutput

Property detail
---------------

.. py:property:: msgl
   :type: int


   
   Get or set the Message level.
   EQ. 0: only time step information is output.
   EQ. 1: first level solver information.
   EQ. 2: full output information with details about linear algebra and convergence steps.
   EQ.4:   full output information is also copied to the message file
















   ..
       !! processed by numpydoc !!

.. py:property:: outl
   :type: int


   
   Get or set the Output the fluid results in other file formats apart from d3plot.
   EQ. 0: only d3plot output
   EQ. 2: output a file with mesh statistics and the fluid results in GMV format. A directory named output/gmv has to be created one level above the executable.
   EQ. 6: output a file with mesh statistics and the fluid results in Paraview format. A directory named vtk will be created in the work directory where the output files will be written.
   EQ.7: output a file with mesh statistic and the fluid results in VTU format readable by Paraview. A directory named vtk will be created in the work directory where the output files will be written.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtout
   :type: float


   
   Get or set the Time interval to print the output when OUTL is different than 0.
















   ..
       !! processed by numpydoc !!

.. py:property:: lsppout
   :type: int


   
   Get or set the EQ.1:    outputs a file with the automatically created fluid volume mesh in  a format compatible for LSPP at each remesh. Also outputs the fluid volume mesh in a format compatible with a subsequent ICFD analysis.
   EQ.3:   Outputs the fluid volume mesh in a format compatible with a subsequent ICFD analysis at each DTOUT
















   ..
       !! processed by numpydoc !!

.. py:property:: itout
   :type: int


   
   Get or set the Iteration interval to print the output, including the d3plot files when the steady state slover is selected
















   ..
       !! processed by numpydoc !!

.. py:property:: pitout
   :type: Optional[int]


   
   Get or set the Pressure iteration limit output. If the number of pressure iterations in the fractional step solve goes above PITOUT, an extra d3plot will be dumped. This is mainly a debugging feature which can help the user identify problematic areas in the model which often precede a divergence
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_OUTPUT'






