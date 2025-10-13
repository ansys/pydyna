





:class:`SectionSph`
===================


.. py:class:: section_sph.SectionSph(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SECTION_SPH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SectionSph

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~secid`
            - Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
          * - :py:attr:`~cslh`
            - Get or set the Constant applied to the smoothing length of the particles.
          * - :py:attr:`~hmin`
            - Get or set the Scale factor for the minimum smoothing length.
          * - :py:attr:`~hmax`
            - Get or set the Scale factor for the maximum smoothing length.
          * - :py:attr:`~sphini`
            - Get or set the Optional initial smoothing length (overrides true smoothing length). This option applies to avoid LS-DYNA to calculate the smoothing length during initialization. In this case, the variable CSLH doesn't apply.
          * - :py:attr:`~death`
            - Get or set the Time imposed SPH approximation is stopped.
          * - :py:attr:`~start`
            - Get or set the Time imposed SPH approximation is activated.
          * - :py:attr:`~sphkern`
            - Get or set the Option for SPH kernel functions (smoothing functions):
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

    from section_sph import SectionSph

Property detail
---------------

.. py:property:: secid
   :type: Optional[int]


   
   Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
















   ..
       !! processed by numpydoc !!

.. py:property:: cslh
   :type: float


   
   Get or set the Constant applied to the smoothing length of the particles.
   The default is set to 1.2. This value applies for most problems.
   Values between 1.05 and 1.3 are acceptable.  Taking a value less than 1 is inadmissible. Values larger than 1.3 will increase the computational time.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmin
   :type: float


   
   Get or set the Scale factor for the minimum smoothing length.
















   ..
       !! processed by numpydoc !!

.. py:property:: hmax
   :type: float


   
   Get or set the Scale factor for the maximum smoothing length.
















   ..
       !! processed by numpydoc !!

.. py:property:: sphini
   :type: float


   
   Get or set the Optional initial smoothing length (overrides true smoothing length). This option applies to avoid LS-DYNA to calculate the smoothing length during initialization. In this case, the variable CSLH doesn't apply.
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time imposed SPH approximation is stopped.
















   ..
       !! processed by numpydoc !!

.. py:property:: start
   :type: float


   
   Get or set the Time imposed SPH approximation is activated.
















   ..
       !! processed by numpydoc !!

.. py:property:: sphkern
   :type: int


   
   Get or set the Option for SPH kernel functions (smoothing functions):
   EQ.0: Cubic spline kernel function (default).
   EQ.1: Quintic spline kernel function: higher order smoothing function with bigger support size (recommend to use
   HMAX = 3.0 or bigger value, only available for FORM = 0, 1, 9 and 10).
















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
   :value: 'SECTION'


.. py:attribute:: subkeyword
   :value: 'SPH'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





