





:class:`BoundaryElementMethodAcoustic`
======================================


.. py:class:: boundary_element_method_acoustic.BoundaryElementMethodAcoustic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_ELEMENT_METHOD_ACOUSTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryElementMethodAcoustic

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ro`
            - Get or set the Fluid Density
          * - :py:attr:`~c`
            - Get or set the Sound speed of the Fluid.
          * - :py:attr:`~fmin`
            - Get or set the Minimum value of output frequencies.
          * - :py:attr:`~fmax`
            - Get or set the Maximum value of output frequencies.
          * - :py:attr:`~nfreq`
            - Get or set the Number of output frequencies.
          * - :py:attr:`~dt_out`
            - Get or set the Time interval between writing velocity or acceleration, and pressure at boundary elements in the binary file, to be proceeded at the end of LS-DYNA simulation
          * - :py:attr:`~t_start`
            - Get or set the Start time for recording velocity or acceleration in LS-DYNA simulation.
          * - :py:attr:`~pref`
            - Get or set the Reference pressure to be used to output pressure in dB, in the file Press_dB. If Ref_Pres=0, the Press_dB file will not be generated. A file called Press_Pa is generated and contains the pressure at the output nodes
          * - :py:attr:`~nsid_ext`
            - Get or set the set ID, or Segment set ID of output exterior field points.
          * - :py:attr:`~type_ext`
            - Get or set the Output exterior field point type.
          * - :py:attr:`~nsid_int`
            - Get or set the Node set ID, or Segment set ID of output interior field points.
          * - :py:attr:`~type_int`
            - Get or set the Output interior field point type.
          * - :py:attr:`~fft_win`
            - Get or set the FFT windows (Default=0).
          * - :py:attr:`~method`
            - Get or set the Method used in acoustic analysis (Default =0)
          * - :py:attr:`~maxit`
            - Get or set the Maximum number of iterations for Iterative solver (Default =100)(Used only if IBEM_Met=2)
          * - :py:attr:`~res`
            - Get or set the Residual for the iterative solver (Default=1.E-6)
          * - :py:attr:`~ndd`
            - Get or set the Number of Domain Decomposition, used for memory saving.For large problems, the boundary mesh is decomposed into NDD domains for less memory allocation.This option is only used if IBEM_Met=2.
          * - :py:attr:`~ssid`
            - Get or set the Part, Part set ID, or Segment set ID of boundary elements.
          * - :py:attr:`~sstype`
            - Get or set the Boundary element type.
          * - :py:attr:`~norm`
            - Get or set the NORM should be set such that the normal vectors face toward the Fluid.
          * - :py:attr:`~bem_type`
            - Get or set the Type of input boundary values in BEM Analysis.
          * - :py:attr:`~restart`
            - Get or set the This Flag is used to save an LS-DYNA analysis if the binary output file in the (bem=filename) option has not been changed. (Default = 0).


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

    from boundary_element_method_acoustic import BoundaryElementMethodAcoustic

Property detail
---------------

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Fluid Density
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Sound speed of the Fluid.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmin
   :type: Optional[float]


   
   Get or set the Minimum value of output frequencies.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: Optional[float]


   
   Get or set the Maximum value of output frequencies.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: Optional[int]


   
   Get or set the Number of output frequencies.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt_out
   :type: Optional[int]


   
   Get or set the Time interval between writing velocity or acceleration, and pressure at boundary elements in the binary file, to be proceeded at the end of LS-DYNA simulation
















   ..
       !! processed by numpydoc !!

.. py:property:: t_start
   :type: Optional[float]


   
   Get or set the Start time for recording velocity or acceleration in LS-DYNA simulation.
















   ..
       !! processed by numpydoc !!

.. py:property:: pref
   :type: Optional[float]


   
   Get or set the Reference pressure to be used to output pressure in dB, in the file Press_dB. If Ref_Pres=0, the Press_dB file will not be generated. A file called Press_Pa is generated and contains the pressure at the output nodes
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid_ext
   :type: Optional[int]


   
   Get or set the set ID, or Segment set ID of output exterior field points.
















   ..
       !! processed by numpydoc !!

.. py:property:: type_ext
   :type: int


   
   Get or set the Output exterior field point type.
   EQ.1:  Node set ID.
   EQ.2:  Segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid_int
   :type: Optional[int]


   
   Get or set the Node set ID, or Segment set ID of output interior field points.
















   ..
       !! processed by numpydoc !!

.. py:property:: type_int
   :type: int


   
   Get or set the Output interior field point type.
   EQ.1:  Node set ID.
   EQ.2:  Segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: fft_win
   :type: int


   
   Get or set the FFT windows (Default=0).
   EQ.0: Rectangular window
   EQ.1: Hanning window
   EQ.2: Hamming window
   EQ.3: Blackman window
   EQ.4: Raised cosine window
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: int


   
   Get or set the Method used in acoustic analysis (Default =0)
   EQ.0:  Rayleigh method (very fast)
   EQ.1: Kirchhoff method coupled to FEM for acoustics (*MAT_ACOUSTIC) (see Remark 1)
   EQ.2:  BEM
















   ..
       !! processed by numpydoc !!

.. py:property:: maxit
   :type: Optional[int]


   
   Get or set the Maximum number of iterations for Iterative solver (Default =100)(Used only if IBEM_Met=2)
















   ..
       !! processed by numpydoc !!

.. py:property:: res
   :type: Optional[float]


   
   Get or set the Residual for the iterative solver (Default=1.E-6)
















   ..
       !! processed by numpydoc !!

.. py:property:: ndd
   :type: Optional[int]


   
   Get or set the Number of Domain Decomposition, used for memory saving.For large problems, the boundary mesh is decomposed into NDD domains for less memory allocation.This option is only used if IBEM_Met=2.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the Part, Part set ID, or Segment set ID of boundary elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: sstype
   :type: int


   
   Get or set the Boundary element type.
   EQ.0:  Part Set ID
   EQ.1:  Part ID
   EQ.2:  Segment set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: norm
   :type: int


   
   Get or set the NORM should be set such that the normal vectors face toward the Fluid.
   EQ.0: Normal vectors are not inverted (Default).
   EQ.1: Normals are inverted.
















   ..
       !! processed by numpydoc !!

.. py:property:: bem_type
   :type: Optional[int]


   
   Get or set the Type of input boundary values in BEM Analysis.
   EQ.0: Boundary velocity will be processed in BEM Analysis
   EQ.1:  Boundary acceleration will be processed in BEM analysis
   EQ.- n:  Velocity is given in frequency domain, through the load curve n. An amplitude vs. frequency load curve (with Curve ID n) needs to be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: restart
   :type: int


   
   Get or set the This Flag is used to save an LS-DYNA analysis if the binary output file in the (bem=filename) option has not been changed. (Default = 0).
   EQ.0:  LS-DYNA analysis is processed and generates a new binary file.
   EQ.1:  LS-DYNA analysis is not processed. The binary file from previous run is used.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'ELEMENT_METHOD_ACOUSTIC'






