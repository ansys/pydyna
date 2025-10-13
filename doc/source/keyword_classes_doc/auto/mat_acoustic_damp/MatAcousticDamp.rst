





:class:`MatAcousticDamp`
========================


.. py:class:: mat_acoustic_damp.MatAcousticDamp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ACOUSTIC_DAMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatAcousticDamp

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
          * - :py:attr:`~cee`
            - Get or set the Sound speed.
          * - :py:attr:`~beta`
            - Get or set the Linear bulk viscosity coefficient.
          * - :py:attr:`~vdc`
            - Get or set the Volumetric drag coefficient.
          * - :py:attr:`~beta2`
            - Get or set the Quadratic bulk viscosity coefficient.
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

    from mat_acoustic_damp import MatAcousticDamp

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

.. py:property:: cee
   :type: Optional[float]


   
   Get or set the Sound speed.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Linear bulk viscosity coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: vdc
   :type: float


   
   Get or set the Volumetric drag coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta2
   :type: float


   
   Get or set the Quadratic bulk viscosity coefficient.
















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
   :value: 'ACOUSTIC_DAMP'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





