





:class:`Contact2DAutomaticTiedOneWay`
=====================================


.. py:class:: contact_2d_automatic_tied_one_way.Contact2DAutomaticTiedOneWay(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTACT_2D_AUTOMATIC_TIED_ONE_WAY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Contact2DAutomaticTiedOneWay

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
          * - :py:attr:`~vc`
            - Get or set the Coefficient for viscous friction. This is used to limit the friction force to a maximum.
          * - :py:attr:`~vdc`
            - Get or set the Viscous damping coefficient in percent of critical for explicit contact.
          * - :py:attr:`~ipf`
            - Get or set the Initial penetration flag for explicit contact.
          * - :py:attr:`~slide`
            - Get or set the Sliding option.
          * - :py:attr:`~istiff`
            - Get or set the Stiffness scaling option.
          * - :py:attr:`~tiedgap`
            - Get or set the Search gap for tied contacts.
          * - :py:attr:`~igapcl`
            - Get or set the Flag to close gaps in tied contact:
          * - :py:attr:`~tietyp`
            - Get or set the Flag to control constraint type of tied contact:
          * - :py:attr:`~sldsoa`
            - Get or set the Solid surface offset for the SURFA surface.
          * - :py:attr:`~sldsob`
            - Get or set the Solid surface offset for the SURFB surface.
          * - :py:attr:`~tdpen`
            - Get or set the Time span of penetration removal for 2D Mortar contacts.


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

    from contact_2d_automatic_tied_one_way import Contact2DAutomaticTiedOneWay

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

.. py:property:: vc
   :type: float


   
   Get or set the Coefficient for viscous friction. This is used to limit the friction force to a maximum.
















   ..
       !! processed by numpydoc !!

.. py:property:: vdc
   :type: float


   
   Get or set the Viscous damping coefficient in percent of critical for explicit contact.
















   ..
       !! processed by numpydoc !!

.. py:property:: ipf
   :type: int


   
   Get or set the Initial penetration flag for explicit contact.
   EQ.0: Allow initial penetrations to remain
   EQ.1: Push apart initially penetrated surfaces.
















   ..
       !! processed by numpydoc !!

.. py:property:: slide
   :type: int


   
   Get or set the Sliding option.
   EQ:0. Off.
   EQ.1: On.
















   ..
       !! processed by numpydoc !!

.. py:property:: istiff
   :type: int


   
   Get or set the Stiffness scaling option.
   EQ.0: Use default option.
   EQ.1: Scale stiffness using segment masses and explicit time step (default for explicit contact).
   EQ.2: Scale stiffness using segment stiffness and dimensions (default for implicit contact)
















   ..
       !! processed by numpydoc !!

.. py:property:: tiedgap
   :type: float


   
   Get or set the Search gap for tied contacts.
   EQ.0: Default, use 1% of the SURFB segment length
   GT.0: Use the input value
   LT.0: Use n% of the SURFB segment length where n=|TIEDGAP|.
















   ..
       !! processed by numpydoc !!

.. py:property:: igapcl
   :type: int


   
   Get or set the Flag to close gaps in tied contact:
   EQ.0: Default, allow gaps to remain
   EQ.1: Move SURFA nodes to SURFB segment to close gaps.
















   ..
       !! processed by numpydoc !!

.. py:property:: tietyp
   :type: int


   
   Get or set the Flag to control constraint type of tied contact:
   EQ.0: Default, use kinematic constraints when possible
   EQ.1: Use only penalty type constraints.
















   ..
       !! processed by numpydoc !!

.. py:property:: sldsoa
   :type: float


   
   Get or set the Solid surface offset for the SURFA surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: sldsob
   :type: float


   
   Get or set the Solid surface offset for the SURFB surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdpen
   :type: float


   
   Get or set the Time span of penetration removal for 2D Mortar contacts.
   Each initial penetration will be gradually reduced linearly in time, so that it is removed by time TDPEN.
   This is the interference option analogue to MPAR1 for IGNORE = 3 in 3D automatic Mortar contacts.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTACT'


.. py:attribute:: subkeyword
   :value: '2D_AUTOMATIC_TIED_ONE_WAY'






