





:class:`FrequencyDomainAcousticFringePlotSphere`
================================================


.. py:class:: frequency_domain_acoustic_fringe_plot_sphere.FrequencyDomainAcousticFringePlotSphere(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA FREQUENCY_DOMAIN_ACOUSTIC_FRINGE_PLOT_SPHERE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: FrequencyDomainAcousticFringePlotSphere

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~center`
            - Get or set the Flag for defining the center point for the sphere.
          * - :py:attr:`~r`
            - Get or set the Radius of the sphere..
          * - :py:attr:`~density`
            - Get or set the Parameter to define how coarse or dense the created sphere mesh is. It is a
          * - :py:attr:`~x`
            - Get or set the x-coordinate of the center.
          * - :py:attr:`~y`
            - Get or set the y-coordinate of the center.
          * - :py:attr:`~z`
            - Get or set the z-coordinate of the center.
          * - :py:attr:`~half1`
            - Get or set the Create a half sphere by trimming the defined sphere (see Remark 3 and Figure 0-1). Note that (x_0,y_0,z_0 ) below is the center of the sphere.
          * - :py:attr:`~half2`
            - Get or set the Create a quarter sphere by trimming the half sphere defined with HALF1 (see Remark 3 and Figure 0-1):


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

    from frequency_domain_acoustic_fringe_plot_sphere import FrequencyDomainAcousticFringePlotSphere

Property detail
---------------

.. py:property:: center
   :type: int


   
   Get or set the Flag for defining the center point for the sphere.
   EQ.1: mass center of the original structure.
   EQ.2: geometry center of the original structure.
   EQ.3: defined by (x, y, z)..
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Radius of the sphere..
















   ..
       !! processed by numpydoc !!

.. py:property:: density
   :type: Optional[int]


   
   Get or set the Parameter to define how coarse or dense the created sphere mesh is. It is a
   number between 3 and 39, where "3" gives you 24 elements while "39"
   gives you 8664 elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the x-coordinate of the center.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the y-coordinate of the center.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the z-coordinate of the center.
















   ..
       !! processed by numpydoc !!

.. py:property:: half1
   :type: int


   
   Get or set the Create a half sphere by trimming the defined sphere (see Remark 3 and Figure 0-1). Note that (x_0,y_0,z_0 ) below is the center of the sphere.
   EQ.0:   A full sphere is created, no trimming
   EQ.1 : Keep x≥x_0
   EQ. - 1 : Keep x≤x_0
   EQ.2 : Keep y≥y_0
   EQ. - 2 : Keep y≤y_0
   EQ.3 : Keep z≥z_0
   EQ. - 3 : Keep z≤z_0
















   ..
       !! processed by numpydoc !!

.. py:property:: half2
   :type: int


   
   Get or set the Create a quarter sphere by trimming the half sphere defined with HALF1 (see Remark 3 and Figure 0-1):
   EQ.0:   No second trimming
   EQ.1 : Keep x≥x_0
   EQ. - 1 : Keep x≤x_0
   EQ.2 : Keep y≥y_0
   EQ. - 2 : Keep y≤y_0
   EQ.3 : Keep z≥z_0
   EQ. - 3 : Keep z≤z_0
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'FREQUENCY'


.. py:attribute:: subkeyword
   :value: 'DOMAIN_ACOUSTIC_FRINGE_PLOT_SPHERE'






