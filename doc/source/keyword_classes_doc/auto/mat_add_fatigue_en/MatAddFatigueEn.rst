





:class:`MatAddFatigueEn`
========================


.. py:class:: mat_add_fatigue_en.MatAddFatigueEn(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ADD_FATIGUE_EN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAddFatigueEn

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification for which the fatigue property applies.
          * - :py:attr:`~kp`
            - Get or set the K^', the cyclic strength coefficient
          * - :py:attr:`~np`
            - Get or set the N^', the cyclic strain hardening exponent
          * - :py:attr:`~sigmaf`
            - Get or set the σ_f^', the fatigue strength coefficient
          * - :py:attr:`~epsp`
            - Get or set the ε_f^', the fatigue ductility coefficient
          * - :py:attr:`~bp`
            - Get or set the b^', the fatigue strength exponent (Basquin’s exponent)
          * - :py:attr:`~cp`
            - Get or set the c^', the fatigue ductility exponent (Coffin-Manson exponent)
          * - :py:attr:`~e`
            - Get or set the Young’s modulus
          * - :py:attr:`~pr`
            - Get or set the Poisson’s ratio.
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

    from mat_add_fatigue_en import MatAddFatigueEn

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification for which the fatigue property applies.
















   ..
       !! processed by numpydoc !!

.. py:property:: kp
   :type: Optional[float]


   
   Get or set the K^', the cyclic strength coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: np
   :type: Optional[float]


   
   Get or set the N^', the cyclic strain hardening exponent
















   ..
       !! processed by numpydoc !!

.. py:property:: sigmaf
   :type: Optional[float]


   
   Get or set the σ_f^', the fatigue strength coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: epsp
   :type: Optional[float]


   
   Get or set the ε_f^', the fatigue ductility coefficient
















   ..
       !! processed by numpydoc !!

.. py:property:: bp
   :type: Optional[float]


   
   Get or set the b^', the fatigue strength exponent (Basquin’s exponent)
















   ..
       !! processed by numpydoc !!

.. py:property:: cp
   :type: Optional[float]


   
   Get or set the c^', the fatigue ductility exponent (Coffin-Manson exponent)
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: Optional[int]


   
   Get or set the Young’s modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson’s ratio.
















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
   :value: 'ADD_FATIGUE_EN'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





