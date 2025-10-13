





:class:`MatViscoelasticHillFoam`
================================


.. py:class:: mat_viscoelastic_hill_foam.MatViscoelasticHillFoam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_VISCOELASTIC_HILL_FOAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatViscoelasticHillFoam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be chosen.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~k`
            - Get or set the Bulk modulus. This modulus is used for determining the contact interface stiffness.
          * - :py:attr:`~n`
            - Get or set the Material constant. Define if LCID=0 below; otherwise, N is fit from the load curve data.
          * - :py:attr:`~nu`
            - Get or set the Damping coefficient.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID that defines the force per unit area versus the stretch ratio. This curve can be given for either uniaxial or biaxial data depending on FITTYPE.
          * - :py:attr:`~fittype`
            - Get or set the Type of fit:
          * - :py:attr:`~lcsr`
            - Get or set the Load curve ID that defines the uniaxial or biaxial stress ratio (see FITTYPE) versus the transverse stretch ratio.
          * - :py:attr:`~lcve`
            - Get or set the Optional load curve ID that defines the relaxation function in shear. This curve is used to fit the coefficients Gi and BETAi. If zero, define the coefficients directly. The latter is recommended.
          * - :py:attr:`~nt`
            - Get or set the Number of terms used to fit the Prony serives, which is a number less than or equal to 12. This number should be equal to the number of decades of time covered by the experimental data. Define this number if LCVE is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
          * - :py:attr:`~gstart`
            - Get or set the Starting value for least square fit. If zero, a default value is set equal to the inverse of the largest time in the experiment. Define this number if LC1 is nonzero, Ci, Material constants. Define up to 8 coefficients.
          * - :py:attr:`~c1`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~c2`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~c3`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~c4`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~c5`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~c6`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~c7`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~c8`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b1`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b2`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b3`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b4`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b5`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b6`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b7`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~b8`
            - Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
          * - :py:attr:`~gi`
            - Get or set the Optional shear relaxation modulus for the ith term.
          * - :py:attr:`~betai`
            - Get or set the Optional decay constant for the ith term
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from mat_viscoelastic_hill_foam import MatViscoelasticHillFoam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be chosen.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Bulk modulus. This modulus is used for determining the contact interface stiffness.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: float


   
   Get or set the Material constant. Define if LCID=0 below; otherwise, N is fit from the load curve data.
















   ..
       !! processed by numpydoc !!

.. py:property:: nu
   :type: float


   
   Get or set the Damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the Load curve ID that defines the force per unit area versus the stretch ratio. This curve can be given for either uniaxial or biaxial data depending on FITTYPE.
















   ..
       !! processed by numpydoc !!

.. py:property:: fittype
   :type: int


   
   Get or set the Type of fit:
   EQ.1:uniaxial data,
   EQ.2:biaxial data.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcsr
   :type: int


   
   Get or set the Load curve ID that defines the uniaxial or biaxial stress ratio (see FITTYPE) versus the transverse stretch ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcve
   :type: int


   
   Get or set the Optional load curve ID that defines the relaxation function in shear. This curve is used to fit the coefficients Gi and BETAi. If zero, define the coefficients directly. The latter is recommended.
















   ..
       !! processed by numpydoc !!

.. py:property:: nt
   :type: float


   
   Get or set the Number of terms used to fit the Prony serives, which is a number less than or equal to 12. This number should be equal to the number of decades of time covered by the experimental data. Define this number if LCVE is nonzero. Carefully check the fit in the D3HSP file to ensure that it is valid, since the least square fit is not always reliable.
















   ..
       !! processed by numpydoc !!

.. py:property:: gstart
   :type: float


   
   Get or set the Starting value for least square fit. If zero, a default value is set equal to the inverse of the largest time in the experiment. Define this number if LC1 is nonzero, Ci, Material constants. Define up to 8 coefficients.
















   ..
       !! processed by numpydoc !!

.. py:property:: c1
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c2
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c3
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c4
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c5
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c6
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c7
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: c8
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b2
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b3
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b4
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b5
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b6
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b7
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: b8
   :type: Optional[float]


   
   Get or set the Material constants. See equations below. Define by to 8 coefficients if LCID=0.
















   ..
       !! processed by numpydoc !!

.. py:property:: gi
   :type: Optional[float]


   
   Get or set the Optional shear relaxation modulus for the ith term.
















   ..
       !! processed by numpydoc !!

.. py:property:: betai
   :type: Optional[float]


   
   Get or set the Optional decay constant for the ith term
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'VISCOELASTIC_HILL_FOAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





