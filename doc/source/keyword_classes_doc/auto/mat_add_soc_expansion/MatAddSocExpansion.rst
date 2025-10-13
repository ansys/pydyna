





:class:`MatAddSocExpansion`
===========================


.. py:class:: mat_add_soc_expansion.MatAddSocExpansion(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_SOC_EXPANSION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddSocExpansion

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for which the SOC expansion property applies
          * - :py:attr:`~lcid`
            - Get or set the For isotropic material models, LCID is the load curve ID defining the SOC expansion coefficient as a function of state of charge. In this case, LCIDY, MULTY, LCIDZ, and MULTZ are ignored. For anisotropic material models, LCID and MULT define the SOC expansion coefficient in the local material a-direction. In either case, if LCID is zero, the SOC expansion coefficient is constant and equal to MULT
          * - :py:attr:`~mult`
            - Get or set the Scale factor scaling load curve given by LCID
          * - :py:attr:`~lcidy`
            - Get or set the Load curve ID defining the SOC expansion coefficient in the local material b-direction as a function of state of charge. If zero, the SOC expansion coefficient in the local material b-direction is constant and equal to MULTY. If MULTY=0 as well, LCID and MULT specify the SOC expansion coefficient in the local material b-direction
          * - :py:attr:`~multy`
            - Get or set the Scale factor scaling load curve given by LCIDY
          * - :py:attr:`~lcidz`
            - Get or set the Load curve ID defining the SOC expansion coefficient in the local material c-direction as a function of state of charge. If zero, the SOC expansion coefficient in the local material c-direction is constant and equal to MULTZ. If MULTZ=0 as well, LCID and MULT specify the SOC expansion coefficient in the local material c-direction
          * - :py:attr:`~multz`
            - Get or set the Scale factor scaling load curve given by LCIDZ
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

    from mat_add_soc_expansion import MatAddSocExpansion

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for which the SOC expansion property applies
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: int


   
   Get or set the For isotropic material models, LCID is the load curve ID defining the SOC expansion coefficient as a function of state of charge. In this case, LCIDY, MULTY, LCIDZ, and MULTZ are ignored. For anisotropic material models, LCID and MULT define the SOC expansion coefficient in the local material a-direction. In either case, if LCID is zero, the SOC expansion coefficient is constant and equal to MULT
















   ..
       !! processed by numpydoc !!

.. py:property:: mult
   :type: Optional[float]


   
   Get or set the Scale factor scaling load curve given by LCID
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: Optional[int]


   
   Get or set the Load curve ID defining the SOC expansion coefficient in the local material b-direction as a function of state of charge. If zero, the SOC expansion coefficient in the local material b-direction is constant and equal to MULTY. If MULTY=0 as well, LCID and MULT specify the SOC expansion coefficient in the local material b-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: multy
   :type: Optional[float]


   
   Get or set the Scale factor scaling load curve given by LCIDY
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: Optional[int]


   
   Get or set the Load curve ID defining the SOC expansion coefficient in the local material c-direction as a function of state of charge. If zero, the SOC expansion coefficient in the local material c-direction is constant and equal to MULTZ. If MULTZ=0 as well, LCID and MULT specify the SOC expansion coefficient in the local material c-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: multz
   :type: Optional[float]


   
   Get or set the Scale factor scaling load curve given by LCIDZ
















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
   :value: 'ADD_SOC_EXPANSION'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





