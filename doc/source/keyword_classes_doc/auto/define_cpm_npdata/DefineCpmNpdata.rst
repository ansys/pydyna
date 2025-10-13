





:class:`DefineCpmNpdata`
========================


.. py:class:: define_cpm_npdata.DefineCpmNpdata(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CPM_NPDATA keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCpmNpdata

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Unique ID for this card
          * - :py:attr:`~hconv`
            - Get or set the Convective heat transfer coefficient used to calculate heat loss from the airbag external surface to ambient. See *AIRBAG_HYBRID developments (Resp. P.O. Marklund).
          * - :py:attr:`~pfric`
            - Get or set the Friction factor F_r if -1.0 < PFRIC ≤ 1.0.  Defaults to FRIC from Card 1 if undefined.  Otherwise,
          * - :py:attr:`~sdfblk`
            - Get or set the Scaling down factor for blockage factor (Default = 1.0, no scaling down).  The valid factor will be (0.0,1.0]. If 0.0, it will set to 1.0.
          * - :py:attr:`~kp`
            - Get or set the Thermal conductivity of the part.
          * - :py:attr:`~inip`
            - Get or set the Place initial air particles on surface.
          * - :py:attr:`~cp`
            - Get or set the Specific heat.
          * - :py:attr:`~psfdcf`
            - Get or set the Additional scale factor for force decay constant.
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

    from define_cpm_npdata import DefineCpmNpdata

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Unique ID for this card
















   ..
       !! processed by numpydoc !!

.. py:property:: hconv
   :type: Optional[float]


   
   Get or set the Convective heat transfer coefficient used to calculate heat loss from the airbag external surface to ambient. See *AIRBAG_HYBRID developments (Resp. P.O. Marklund).
   LT.0: | HCONV | is a load curve ID defines heat convection coefficient as a function of time.
















   ..
       !! processed by numpydoc !!

.. py:property:: pfric
   :type: Optional[float]


   
   Get or set the Friction factor F_r if -1.0 < PFRIC ≤ 1.0.  Defaults to FRIC from Card 1 if undefined.  Otherwise,
   LE. - 1.0: | PFRIC | is the curve ID which defines F_r as a function of the part pressure.
   GT.1.0 : PFRIC is the * DEFINE_FUNCTION ID that defines F_r.
















   ..
       !! processed by numpydoc !!

.. py:property:: sdfblk
   :type: Optional[float]


   
   Get or set the Scaling down factor for blockage factor (Default = 1.0, no scaling down).  The valid factor will be (0.0,1.0]. If 0.0, it will set to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: kp
   :type: Optional[float]


   
   Get or set the Thermal conductivity of the part.
















   ..
       !! processed by numpydoc !!

.. py:property:: inip
   :type: Optional[int]


   
   Get or set the Place initial air particles on surface.
   EQ.0:   yes(default)
   EQ.1 : no
   This feature excludes surfaces from initial particle placement.This option is useful for preventing particles from being trapped between adjacent fabric layers.
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the Specific heat.
















   ..
       !! processed by numpydoc !!

.. py:property:: psfdcf
   :type: Optional[float]


   
   Get or set the Additional scale factor for force decay constant.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'CPM_NPDATA'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





