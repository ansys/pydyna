





:class:`MatDruckerPrager`
=========================


.. py:class:: mat_drucker_prager.MatDruckerPrager(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_DRUCKER_PRAGER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatDruckerPrager

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~gmod`
            - Get or set the Elastic shear modulus.
          * - :py:attr:`~rnu`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~rkf`
            - Get or set the Failure surface shape parameter.
          * - :py:attr:`~phi`
            - Get or set the Angle of friction (in radians).
          * - :py:attr:`~cval`
            - Get or set the Cohesion value.
          * - :py:attr:`~psi`
            - Get or set the Dilation angle (in radians).
          * - :py:attr:`~str_lim`
            - Get or set the Minimum shear strength of material is given by STR_LIM*CVAL.
          * - :py:attr:`~gmoddp`
            - Get or set the Depth at which shear modulus (GMOD) is correct.
          * - :py:attr:`~phidp`
            - Get or set the Depth at which angle of friction (PHI) is correct.
          * - :py:attr:`~cvaldp`
            - Get or set the Depth at which cohesion value (CVAL) is correct.
          * - :py:attr:`~psidp`
            - Get or set the Depth at which dilation angle (PSI) is correct.
          * - :py:attr:`~gmodgr`
            - Get or set the Gradient at which shear modulus (GMOD) increases with depth.
          * - :py:attr:`~phigr`
            - Get or set the Gradient at which friction angle (PHI) increases with depth.
          * - :py:attr:`~cvalgr`
            - Get or set the Gradient at which cohesion value (CVAL) increases with depth.
          * - :py:attr:`~psigr`
            - Get or set the Gradient at which dilation angle (PSI) increases with depth.
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

    from mat_drucker_prager import MatDruckerPrager

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmod
   :type: Optional[float]


   
   Get or set the Elastic shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: rnu
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: rkf
   :type: float


   
   Get or set the Failure surface shape parameter.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: phi
   :type: Optional[float]


   
   Get or set the Angle of friction (in radians).
















   ..
       !! processed by numpydoc !!

.. py:property:: cval
   :type: Optional[float]


   
   Get or set the Cohesion value.
















   ..
       !! processed by numpydoc !!

.. py:property:: psi
   :type: Optional[float]


   
   Get or set the Dilation angle (in radians).
   Default is set to 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: str_lim
   :type: float


   
   Get or set the Minimum shear strength of material is given by STR_LIM*CVAL.
   Default is set to 5.0E-03
















   ..
       !! processed by numpydoc !!

.. py:property:: gmoddp
   :type: Optional[float]


   
   Get or set the Depth at which shear modulus (GMOD) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: phidp
   :type: Optional[float]


   
   Get or set the Depth at which angle of friction (PHI) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvaldp
   :type: Optional[float]


   
   Get or set the Depth at which cohesion value (CVAL) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: psidp
   :type: Optional[float]


   
   Get or set the Depth at which dilation angle (PSI) is correct.
















   ..
       !! processed by numpydoc !!

.. py:property:: gmodgr
   :type: Optional[float]


   
   Get or set the Gradient at which shear modulus (GMOD) increases with depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: phigr
   :type: Optional[float]


   
   Get or set the Gradient at which friction angle (PHI) increases with depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: cvalgr
   :type: Optional[float]


   
   Get or set the Gradient at which cohesion value (CVAL) increases with depth.
















   ..
       !! processed by numpydoc !!

.. py:property:: psigr
   :type: Optional[float]


   
   Get or set the Gradient at which dilation angle (PSI) increases with depth.
















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
   :value: 'DRUCKER_PRAGER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





