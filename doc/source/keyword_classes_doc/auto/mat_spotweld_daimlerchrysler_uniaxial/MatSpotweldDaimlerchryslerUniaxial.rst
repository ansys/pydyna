





:class:`MatSpotweldDaimlerchryslerUniaxial`
===========================================


.. py:class:: mat_spotweld_daimlerchrysler_uniaxial.MatSpotweldDaimlerchryslerUniaxial(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SPOTWELD_DAIMLERCHRYSLER_UNIAXIAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSpotweldDaimlerchryslerUniaxial

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus: LT.0.0: |"E"| is the Young's modulus. E < 0 invokes uniaxial stress for solid spot welds with the transverse stresses assumed to be zero. See Remark 1.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~dt`
            - Get or set the Time step size for mass scaling, Delta t.
          * - :py:attr:`~tfail`
            - Get or set the Failure time if nonzero. If zero this option is ignored.
          * - :py:attr:`~efail`
            - Get or set the Effective plastic strain in weld material at failure.  The spot weld element is deleted when the plastic strain at each integration point exceeds EFAIL.  If zero, failure due to effective plastic strain is not considered.
          * - :py:attr:`~nf`
            - Get or set the Number of force vectors stored for filtering (default = 0). Default is recommended unless oscillatory resultant forces are observed in the time history databases. Even though these welds should not oscillate significantly, this option was added for consistency with the other spot weld options. NF affects the storage since it is necessary to store the resultant forces as history variables. When NF is nonzero, the resultants in the output databases are filtered.
          * - :py:attr:`~rs`
            - Get or set the Rupture strain.  See Remark 2
          * - :py:attr:`~asff`
            - Get or set the Weld assembly simultaneous failure flag (see Remark 4):
          * - :py:attr:`~true_t`
            - Get or set the True weld thickness for single hexahedron solid weld elements. See Remark 3
          * - :py:attr:`~con_id`
            - Get or set the Connection ID of *DEFINE_CONNECTION card. A negative CON_ID deactivates failure; see Remark 6
          * - :py:attr:`~rfiltf`
            - Get or set the Smoothing factor on the effective strain rate (default is 0.0), potentially used in table DSIGY<0 and in functions for PRUL.ge.2 (see *DEFINE_CONNECTION_PROPERTIES).
          * - :py:attr:`~jtol`
            - Get or set the Tolerance value for relative volume change (default: JTOL = 0.01). Solid element spot welds with a Jacobian less than JTOL will be eroded
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

    from mat_spotweld_daimlerchrysler_uniaxial import MatSpotweldDaimlerchryslerUniaxial

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus: LT.0.0: |"E"| is the Young's modulus. E < 0 invokes uniaxial stress for solid spot welds with the transverse stresses assumed to be zero. See Remark 1.
   This is for when the keyword option is unset (<BLANK>) only..
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Time step size for mass scaling, Delta t.
















   ..
       !! processed by numpydoc !!

.. py:property:: tfail
   :type: Optional[float]


   
   Get or set the Failure time if nonzero. If zero this option is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: efail
   :type: Optional[float]


   
   Get or set the Effective plastic strain in weld material at failure.  The spot weld element is deleted when the plastic strain at each integration point exceeds EFAIL.  If zero, failure due to effective plastic strain is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: nf
   :type: Optional[float]


   
   Get or set the Number of force vectors stored for filtering (default = 0). Default is recommended unless oscillatory resultant forces are observed in the time history databases. Even though these welds should not oscillate significantly, this option was added for consistency with the other spot weld options. NF affects the storage since it is necessary to store the resultant forces as history variables. When NF is nonzero, the resultants in the output databases are filtered.
















   ..
       !! processed by numpydoc !!

.. py:property:: rs
   :type: Optional[float]


   
   Get or set the Rupture strain.  See Remark 2
















   ..
       !! processed by numpydoc !!

.. py:property:: asff
   :type: Optional[int]


   
   Get or set the Weld assembly simultaneous failure flag (see Remark 4):
   EQ.0:   Damaged elements fail individually.
   EQ.1 : Damaged elements fail when first reaches failure criterion.
















   ..
       !! processed by numpydoc !!

.. py:property:: true_t
   :type: Optional[float]


   
   Get or set the True weld thickness for single hexahedron solid weld elements. See Remark 3
















   ..
       !! processed by numpydoc !!

.. py:property:: con_id
   :type: Optional[int]


   
   Get or set the Connection ID of *DEFINE_CONNECTION card. A negative CON_ID deactivates failure; see Remark 6
















   ..
       !! processed by numpydoc !!

.. py:property:: rfiltf
   :type: Optional[float]


   
   Get or set the Smoothing factor on the effective strain rate (default is 0.0), potentially used in table DSIGY<0 and in functions for PRUL.ge.2 (see *DEFINE_CONNECTION_PROPERTIES).
















   ..
       !! processed by numpydoc !!

.. py:property:: jtol
   :type: Optional[float]


   
   Get or set the Tolerance value for relative volume change (default: JTOL = 0.01). Solid element spot welds with a Jacobian less than JTOL will be eroded
















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
   :value: 'SPOTWELD_DAIMLERCHRYSLER_UNIAXIAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





