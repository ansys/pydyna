





:class:`PerturbationNode`
=========================


.. py:class:: perturbation_node.PerturbationNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PERTURBATION_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PerturbationNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~type`
            - Get or set the Type of perturbation.
          * - :py:attr:`~nsid`
            - Get or set the Node set ID. Specify 0 to perturb all the nodes in the model.
          * - :py:attr:`~scl`
            - Get or set the Scale factor.
          * - :py:attr:`~cmp`
            - Get or set the Component. For the NODE option, these are given below. For the MATERIAL option, see the description of the material.
          * - :py:attr:`~icoord`
            - Get or set the Coordinate system to use;
          * - :py:attr:`~cid`
            - Get or set the Coordinate system ID.see *DEFINE_‌COORDINATE_‌NODES
          * - :py:attr:`~ampl`
            - Get or set the Amplitude of the harmonic perturbation.
          * - :py:attr:`~xwl`
            - Get or set the x wavelength of the harmonic field.
          * - :py:attr:`~xoff`
            - Get or set the x offset of harmonic field.
          * - :py:attr:`~ywl`
            - Get or set the y wavelength of the harmonic field.
          * - :py:attr:`~yoff`
            - Get or set the y offset of harmonic field.
          * - :py:attr:`~zwl`
            - Get or set the z wavelength of the harmonic field.
          * - :py:attr:`~zoff`
            - Get or set the z offset of harmonic field.
          * - :py:attr:`~fade`
            - Get or set the Distance over which all *PERTURBATION_NODE are faded to zero.
          * - :py:attr:`~fname`
            - Get or set the Name of file containing the perturbation definitions.
          * - :py:attr:`~cstype`
            - Get or set the Correlation structure:
          * - :py:attr:`~ellip1`
            - Get or set the Elliptic constant for 2D and 3D elliptic fields.
          * - :py:attr:`~ellip2`
            - Get or set the Elliptic constant for 3D elliptic field.
          * - :py:attr:`~rnd`
            - Get or set the Seed for random number generator.
          * - :py:attr:`~cftype`
            - Get or set the Correlation function
          * - :py:attr:`~cfc1`
            - Get or set the Correlation function constant 1.
          * - :py:attr:`~cfc2`
            - Get or set the Correlation function constant 2.
          * - :py:attr:`~cfc3`
            - Get or set the Correlation function constant 3.
          * - :py:attr:`~dtype`
            - Get or set the Distribution type:


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

    from perturbation_node import PerturbationNode

Property detail
---------------

.. py:property:: type
   :type: int


   
   Get or set the Type of perturbation.
   EQ.1:   Harmonic Field (see Remark 3)
   EQ.2:   Fade out all perturbations at this node set(see Remark 4)
   EQ.3 : Read perturbations from a file
   EQ.4 : Spectral field
   EQ.8 : Random value from uniform distribution
















   ..
       !! processed by numpydoc !!

.. py:property:: nsid
   :type: int


   
   Get or set the Node set ID. Specify 0 to perturb all the nodes in the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: scl
   :type: float


   
   Get or set the Scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: cmp
   :type: int


   
   Get or set the Component. For the NODE option, these are given below. For the MATERIAL option, see the description of the material.
   EQ.1: x coordinate
   EQ.2: y coordinate
   EQ.3: z coordinate
   EQ.4: x and y coordinate
   EQ.5: y and z coordinate
   EQ.6: z and x coordinate
   EQ.7: x, y, and z coordinate
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: icoord
   :type: int


   
   Get or set the Coordinate system to use;
   EQ.0: Global Cartesian
   EQ.1: Cartesian
   EQ.2: Cylindrical (computed and applied)
   EQ.3: Spherical (computed and applied)
   EQ.-2: Computed in cartesian but applied in cylindrical
   EQ.-3  Computed in cartesian but applied in spherical.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: int


   
   Get or set the Coordinate system ID.see *DEFINE_‌COORDINATE_‌NODES
















   ..
       !! processed by numpydoc !!

.. py:property:: ampl
   :type: float


   
   Get or set the Amplitude of the harmonic perturbation.
















   ..
       !! processed by numpydoc !!

.. py:property:: xwl
   :type: float


   
   Get or set the x wavelength of the harmonic field.
















   ..
       !! processed by numpydoc !!

.. py:property:: xoff
   :type: float


   
   Get or set the x offset of harmonic field.
















   ..
       !! processed by numpydoc !!

.. py:property:: ywl
   :type: float


   
   Get or set the y wavelength of the harmonic field.
















   ..
       !! processed by numpydoc !!

.. py:property:: yoff
   :type: float


   
   Get or set the y offset of harmonic field.
















   ..
       !! processed by numpydoc !!

.. py:property:: zwl
   :type: float


   
   Get or set the z wavelength of the harmonic field.
















   ..
       !! processed by numpydoc !!

.. py:property:: zoff
   :type: float


   
   Get or set the z offset of harmonic field.
















   ..
       !! processed by numpydoc !!

.. py:property:: fade
   :type: float


   
   Get or set the Distance over which all *PERTURBATION_NODE are faded to zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: fname
   :type: Optional[str]


   
   Get or set the Name of file containing the perturbation definitions.
















   ..
       !! processed by numpydoc !!

.. py:property:: cstype
   :type: int


   
   Get or set the Correlation structure:
   EQ.1: 3D isotropic. The X, Y and Z correlations are described using one correlation function.
   EQ.2: 3D product. The X, Y and Z correlations are described using a correlation function each.
   EQ.3: 2D isotropic. A correlation function describes the X correlation while the YZ isotropic relationship is described using another correlation function.
   EQ.4: 2D isotropic. A correlation function describes the Y correlation while the XZ isotropic relationship is described using another correlation function.
   EQ.5: 2D isotropic. A correlation function describes the Z correlation while the XY isotropic relationship is described using another correlation function.
   EQ.6: 3D elliptic. Define CSE1 and CSE2.
   EQ.7: 2D elliptic. A correlation function describes the X correlation while the YZ elliptic relationship is described using another correlation function.
   EQ.8: 2D elliptic. A correlation function describes the Y correlation while the ZX elliptic relationship is described using another correlation function.
   EQ.9: 2D elliptic. A correlation function describes the Z correlation while the XY elliptic relationship is described using another correlation function.
















   ..
       !! processed by numpydoc !!

.. py:property:: ellip1
   :type: float


   
   Get or set the Elliptic constant for 2D and 3D elliptic fields.
















   ..
       !! processed by numpydoc !!

.. py:property:: ellip2
   :type: float


   
   Get or set the Elliptic constant for 3D elliptic field.
















   ..
       !! processed by numpydoc !!

.. py:property:: rnd
   :type: int


   
   Get or set the Seed for random number generator.
   EQ.0: LS-DYNA will generate a random seed
   GT.0: Value to be used as seed.
















   ..
       !! processed by numpydoc !!

.. py:property:: cftype
   :type: int


   
   Get or set the Correlation function
   EQ.1: Gaussian
   EQ.2: Exponential
   EQ.3: Exponential Cosine
   EQ.4: Rational
   EQ.5: Linear.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfc1
   :type: float


   
   Get or set the Correlation function constant 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfc2
   :type: float


   
   Get or set the Correlation function constant 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: cfc3
   :type: float


   
   Get or set the Correlation function constant 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: dtype
   :type: float


   
   Get or set the Distribution type:
   EQ.0.0: Uniform distribution between SCL×[0,AMPL]
   EQ.1.0 : Uniform distribution between SCL×[-AMPL ,AMPL]
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PERTURBATION'


.. py:attribute:: subkeyword
   :value: 'NODE'






