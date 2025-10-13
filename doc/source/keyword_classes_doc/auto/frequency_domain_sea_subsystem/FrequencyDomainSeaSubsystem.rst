





:class:`FrequencyDomainSeaSubsystem`
====================================


.. py:class:: frequency_domain_sea_subsystem.FrequencyDomainSeaSubsystem(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_SEA_SUBSYSTEM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainSeaSubsystem

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fmin`
            - Get or set the Minimum frequency for SEA output (cycles/time).
          * - :py:attr:`~fmax`
            - Get or set the Maximum frequency for SEA output (cycles/time).
          * - :py:attr:`~nfreq`
            - Get or set the Number of frequencies for SEA output (cycles/time).
          * - :py:attr:`~nfspace`
            - Get or set the Frequency spacing option for SEA output:
          * - :py:attr:`~lcfreq`
            - Get or set the Load Curve ID defining the frequencies for SEA output.
          * - :py:attr:`~iread`
            - Get or set the Type of SEA run:
          * - :py:attr:`~subid`
            - Get or set the ID of subsystem.
          * - :py:attr:`~subtyp`
            - Get or set the Type of subsystem:
          * - :py:attr:`~density`
            - Get or set the Mass density of subsystem.
          * - :py:attr:`~e`
            - Get or set the Young's modulus of subsystem.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio of subsystem.
          * - :py:attr:`~output`
            - Get or set the Include this subsystem in output:
          * - :py:attr:`~a`
            - Get or set the Plate Area.
          * - :py:attr:`~perim`
            - Get or set the Plate Perimeter.
          * - :py:attr:`~thick`
            - Get or set the Plate Thickness.
          * - :py:attr:`~width`
            - Get or set the Plate width.
          * - :py:attr:`~length`
            - Get or set the Plate length.
          * - :py:attr:`~dampb`
            - Get or set the Damping factor for bending wave.
          * - :py:attr:`~dampl`
            - Get or set the Damping factor for longitudinal wave.
          * - :py:attr:`~damps`
            - Get or set the Damping factor for shear wave.
          * - :py:attr:`~lc1`
            - Get or set the Load curve for damping factor for bending wave.
          * - :py:attr:`~lc2`
            - Get or set the Load curve for damping factor for longitudinal wave.
          * - :py:attr:`~lc3`
            - Get or set the Load curve for damping factor for shear wave.
          * - :py:attr:`~volume`
            - Get or set the Cavity volume.
          * - :py:attr:`~height`
            - Get or set the Cavity height.
          * - :py:attr:`~iss`
            - Get or set the area moment of inertia about local s-axis.
          * - :py:attr:`~itt`
            - Get or set the area moment of inertia about local t-axis.
          * - :py:attr:`~j`
            - Get or set the torsional constant.
          * - :py:attr:`~dampt`
            - Get or set the Damping factor for torsional wave
          * - :py:attr:`~lc4`
            - Get or set the Load curve for damping factor for torsional wave


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

    from frequency_domain_sea_subsystem import FrequencyDomainSeaSubsystem

Property detail
---------------

.. py:property:: fmin
   :type: Optional[float]


   
   Get or set the Minimum frequency for SEA output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: Optional[float]


   
   Get or set the Maximum frequency for SEA output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: Optional[int]


   
   Get or set the Number of frequencies for SEA output (cycles/time).
















   ..
       !! processed by numpydoc !!

.. py:property:: nfspace
   :type: int


   
   Get or set the Frequency spacing option for SEA output:
   EQ.0: linear
   EQ.1: logarithmic
   EQ.2: biased
















   ..
       !! processed by numpydoc !!

.. py:property:: lcfreq
   :type: Optional[int]


   
   Get or set the Load Curve ID defining the frequencies for SEA output.
















   ..
       !! processed by numpydoc !!

.. py:property:: iread
   :type: int


   
   Get or set the Type of SEA run:
   EQ.0:   run SEA analysis.
   EQ.1 : read FEM keyword input deck and create SEA model..
















   ..
       !! processed by numpydoc !!

.. py:property:: subid
   :type: Optional[int]


   
   Get or set the ID of subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: subtyp
   :type: int


   
   Get or set the Type of subsystem:
   EQ.1: plate
   EQ.2: cavity
   EQ.3: beam.
















   ..
       !! processed by numpydoc !!

.. py:property:: density
   :type: Optional[float]


   
   Get or set the Mass density of subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus of subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio of subsystem.
















   ..
       !! processed by numpydoc !!

.. py:property:: output
   :type: int


   
   Get or set the Include this subsystem in output:
   EQ.0:   no
   EQ.1 : yes.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Plate Area.
















   ..
       !! processed by numpydoc !!

.. py:property:: perim
   :type: Optional[float]


   
   Get or set the Plate Perimeter.
















   ..
       !! processed by numpydoc !!

.. py:property:: thick
   :type: Optional[float]


   
   Get or set the Plate Thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: width
   :type: Optional[float]


   
   Get or set the Plate width.
















   ..
       !! processed by numpydoc !!

.. py:property:: length
   :type: Optional[float]


   
   Get or set the Plate length.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampb
   :type: Optional[float]


   
   Get or set the Damping factor for bending wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampl
   :type: Optional[float]


   
   Get or set the Damping factor for longitudinal wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: damps
   :type: Optional[float]


   
   Get or set the Damping factor for shear wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc1
   :type: int


   
   Get or set the Load curve for damping factor for bending wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc2
   :type: int


   
   Get or set the Load curve for damping factor for longitudinal wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc3
   :type: int


   
   Get or set the Load curve for damping factor for shear wave.
















   ..
       !! processed by numpydoc !!

.. py:property:: volume
   :type: Optional[float]


   
   Get or set the Cavity volume.
















   ..
       !! processed by numpydoc !!

.. py:property:: height
   :type: Optional[int]


   
   Get or set the Cavity height.
















   ..
       !! processed by numpydoc !!

.. py:property:: iss
   :type: Optional[float]


   
   Get or set the area moment of inertia about local s-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: itt
   :type: Optional[float]


   
   Get or set the area moment of inertia about local t-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: j
   :type: Optional[float]


   
   Get or set the torsional constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: dampt
   :type: Optional[float]


   
   Get or set the Damping factor for torsional wave
















   ..
       !! processed by numpydoc !!

.. py:property:: lc4
   :type: int


   
   Get or set the Load curve for damping factor for torsional wave
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_SEA_SUBSYSTEM'






