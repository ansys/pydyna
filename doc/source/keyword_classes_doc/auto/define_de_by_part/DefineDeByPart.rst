





:class:`DefineDeByPart`
=======================


.. py:class:: define_de_by_part.DefineDeByPart(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_DE_BY_PART keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineDeByPart

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of DES nodes.
          * - :py:attr:`~ndamp`
            - Get or set the Normal damping coefficient.
          * - :py:attr:`~tdamp`
            - Get or set the Tangential damping coefficient.
          * - :py:attr:`~fric`
            - Get or set the Friction coefficient
          * - :py:attr:`~fricr`
            - Get or set the Rolling friction coefficient.
          * - :py:attr:`~normk`
            - Get or set the Optional: scale factor of normal spring constant (Default = 0.01).
          * - :py:attr:`~sheark`
            - Get or set the Optional: ratio between ShearK/NormK (Default = 2/7).
          * - :py:attr:`~gamma`
            - Get or set the Liquid surface tension.
          * - :py:attr:`~vol`
            - Get or set the Volume fraction.
          * - :py:attr:`~ang`
            - Get or set the Contact angle.
          * - :py:attr:`~lnorm`
            - Get or set the Load curve ID of a curve that defines normal stiffness as a function of norm penetration ratio
          * - :py:attr:`~lshear`
            - Get or set the Load curve ID of a curve that defines shear stiffness as a function of norm penetration ratio
          * - :py:attr:`~fricd`
            - Get or set the Dynamic coefficient of friction. By default, FRICD = FRICS.
          * - :py:attr:`~dc`
            - Get or set the Exponential decay coefficient.  See Remarks
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

    from define_de_by_part import DefineDeByPart

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID of DES nodes.
















   ..
       !! processed by numpydoc !!

.. py:property:: ndamp
   :type: float


   
   Get or set the Normal damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdamp
   :type: float


   
   Get or set the Tangential damping coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: fric
   :type: float


   
   Get or set the Friction coefficient
   EQ.0: 3 DOF
   NE.0: 6 DOF (consider rotational DOF).
















   ..
       !! processed by numpydoc !!

.. py:property:: fricr
   :type: float


   
   Get or set the Rolling friction coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: normk
   :type: float


   
   Get or set the Optional: scale factor of normal spring constant (Default = 0.01).
















   ..
       !! processed by numpydoc !!

.. py:property:: sheark
   :type: float


   
   Get or set the Optional: ratio between ShearK/NormK (Default = 2/7).
















   ..
       !! processed by numpydoc !!

.. py:property:: gamma
   :type: float


   
   Get or set the Liquid surface tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: vol
   :type: float


   
   Get or set the Volume fraction.
















   ..
       !! processed by numpydoc !!

.. py:property:: ang
   :type: float


   
   Get or set the Contact angle.
















   ..
       !! processed by numpydoc !!

.. py:property:: lnorm
   :type: int


   
   Get or set the Load curve ID of a curve that defines normal stiffness as a function of norm penetration ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: lshear
   :type: int


   
   Get or set the Load curve ID of a curve that defines shear stiffness as a function of norm penetration ratio
















   ..
       !! processed by numpydoc !!

.. py:property:: fricd
   :type: float


   
   Get or set the Dynamic coefficient of friction. By default, FRICD = FRICS.
















   ..
       !! processed by numpydoc !!

.. py:property:: dc
   :type: float


   
   Get or set the Exponential decay coefficient.  See Remarks
















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
   :value: 'DE_BY_PART'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





