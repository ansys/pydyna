





:class:`MatSmoothViscoelasticViscoplastic`
==========================================


.. py:class:: mat_smooth_viscoelastic_viscoplastic.MatSmoothViscoelasticViscoplastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_SMOOTH_VISCOELASTIC_VISCOPLASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatSmoothViscoelasticViscoplastic

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
            - Get or set the Mass Density
          * - :py:attr:`~k`
            - Get or set the Elastic bulk modulus
          * - :py:attr:`~a0`
            - Get or set the Rate dependent understress viscoplastic parameter.
          * - :py:attr:`~b0`
            - Get or set the Rate independent understress plasticity parameter.
          * - :py:attr:`~a1`
            - Get or set the Rate dependent overstress viscoplastic parameter.
          * - :py:attr:`~b1`
            - Get or set the Rate independent overstress plasticity parameter.
          * - :py:attr:`~m`
            - Get or set the Exponential hardening parameter
          * - :py:attr:`~kapas`
            - Get or set the Saturated yield strain.
          * - :py:attr:`~kapa0`
            - Get or set the Initial yield strain.
          * - :py:attr:`~shear`
            - Get or set the Elastic shear modulus.
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

    from mat_smooth_viscoelastic_viscoplastic import MatSmoothViscoelasticViscoplastic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass Density
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Elastic bulk modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: a0
   :type: Optional[float]


   
   Get or set the Rate dependent understress viscoplastic parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: b0
   :type: Optional[float]


   
   Get or set the Rate independent understress plasticity parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Rate dependent overstress viscoplastic parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: b1
   :type: Optional[float]


   
   Get or set the Rate independent overstress plasticity parameter.
















   ..
       !! processed by numpydoc !!

.. py:property:: m
   :type: Optional[float]


   
   Get or set the Exponential hardening parameter
















   ..
       !! processed by numpydoc !!

.. py:property:: kapas
   :type: Optional[float]


   
   Get or set the Saturated yield strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: kapa0
   :type: Optional[float]


   
   Get or set the Initial yield strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: shear
   :type: Optional[float]


   
   Get or set the Elastic shear modulus.
















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
   :value: 'SMOOTH_VISCOELASTIC_VISCOPLASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





