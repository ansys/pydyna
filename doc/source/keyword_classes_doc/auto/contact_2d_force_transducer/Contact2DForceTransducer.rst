





:class:`Contact2DForceTransducer`
=================================


.. py:class:: contact_2d_force_transducer.Contact2DForceTransducer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_2D_FORCE_TRANSDUCER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Contact2DForceTransducer

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~surfa`
            - Get or set the Set ID for SURFA.  If SURFA > 0, a part set is assumed; see *SET_‌PART.  If SURFA < 0, a node set with ID equal to the absolute value of SURFA is assumed; see *SET_‌NODE. For nonsymmetric contact, this surface is the tracked surface.
          * - :py:attr:`~surfb`
            - Get or set the Set ID to define the SURFB surface.  If SURFB > 0, a part set is assumed; see *SET_‌PART.  If SURFB < 0, a node set with ID equal to the absolute value of SURFB is assumed; see *SET_‌NODE.  Do not define for single surface contact. For nonsymmetric contact, this surface is the reference surface.
          * - :py:attr:`~sfact`
            - Get or set the Scale factor for the penalty force stiffness (default=1.0).
          * - :py:attr:`~freq`
            - Get or set the Search frequency. The number of time steps between bucket sorts (default=50).
          * - :py:attr:`~fs`
            - Get or set the Static coefficient of friction (default=0.0).
          * - :py:attr:`~fd`
            - Get or set the Dynamic coefficient of friction (default=0.0).
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient (default=0.0).
          * - :py:attr:`~tbirth`
            - Get or set the Birth time for contact (default=0.0).
          * - :py:attr:`~tdeath`
            - Get or set the Death time for contact (default=1.0E+20).
          * - :py:attr:`~soa`
            - Get or set the Surface offset from midline for 2D shells of SURFA surface:
          * - :py:attr:`~sob`
            - Get or set the Surface offset from midline for 2D shells of SURFB surface:
          * - :py:attr:`~nda`
            - Get or set the Normal direction flag for 2D shells of SURFA surface:
          * - :py:attr:`~ndb`
            - Get or set the Normal direction flag for 2D shells of SURFB surface:
          * - :py:attr:`~cof`
            - Get or set the COF: Closing/opening flag for implicit analysis.
          * - :py:attr:`~init`
            - Get or set the Special processing during initialization.


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

    from contact_2d_force_transducer import Contact2DForceTransducer

Property detail
---------------

.. py:property:: surfa
   :type: Optional[int]


   
   Get or set the Set ID for SURFA.  If SURFA > 0, a part set is assumed; see *SET_‌PART.  If SURFA < 0, a node set with ID equal to the absolute value of SURFA is assumed; see *SET_‌NODE. For nonsymmetric contact, this surface is the tracked surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: surfb
   :type: Optional[int]


   
   Get or set the Set ID to define the SURFB surface.  If SURFB > 0, a part set is assumed; see *SET_‌PART.  If SURFB < 0, a node set with ID equal to the absolute value of SURFB is assumed; see *SET_‌NODE.  Do not define for single surface contact. For nonsymmetric contact, this surface is the reference surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: sfact
   :type: float


   
   Get or set the Scale factor for the penalty force stiffness (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: freq
   :type: int


   
   Get or set the Search frequency. The number of time steps between bucket sorts (default=50).
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: float


   
   Get or set the Static coefficient of friction (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: fd
   :type: float


   
   Get or set the Dynamic coefficient of friction (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Exponential decay coefficient (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: tbirth
   :type: float


   
   Get or set the Birth time for contact (default=0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdeath
   :type: float


   
   Get or set the Death time for contact (default=1.0E+20).
















   ..
       !! processed by numpydoc !!

.. py:property:: soa
   :type: float


   
   Get or set the Surface offset from midline for 2D shells of SURFA surface:
   GT.0.0: scale factor applied to actual thickness,
   LT.0.0: absolute value is used as the offset.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: sob
   :type: float


   
   Get or set the Surface offset from midline for 2D shells of SURFB surface:
   GT.0.0: scale factor applied to actual thickness,
   LT.0.0: absolute value is used as the offset.
   Default is set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: nda
   :type: int


   
   Get or set the Normal direction flag for 2D shells of SURFA surface:
   EQ.0: Normal direction is determined automatically (default),
   EQ.1: Normal direction is in the positive direction,
   EQ.-1: Normal direction is in the negative direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndb
   :type: int


   
   Get or set the Normal direction flag for 2D shells of SURFB surface:
   EQ.0: Normal direction is determined automatically (default),
   EQ.1: Normal direction is in the positive direction,
   EQ.-1: Normal direction is in the negative direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: cof
   :type: int


   
   Get or set the COF: Closing/opening flag for implicit analysis.
   EQ.0: Recommended for most problems where gaps are only closing (default),
   EQ.1: Recommended when gaps are opening to avoid sticking.
















   ..
       !! processed by numpydoc !!

.. py:property:: init
   :type: int


   
   Get or set the Special processing during initialization.
   EQ.0: No special processing,
   EQ.1: Forming option.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: '2D_FORCE_TRANSDUCER'






