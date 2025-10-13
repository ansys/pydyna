





:class:`MatIsotropicElasticFailure`
===================================


.. py:class:: mat_isotropic_elastic_failure.MatIsotropicElasticFailure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_ISOTROPIC_ELASTIC_FAILURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatIsotropicElasticFailure

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
          * - :py:attr:`~g`
            - Get or set the Shear modulus.
          * - :py:attr:`~sigy`
            - Get or set the Yield stress.
          * - :py:attr:`~etan`
            - Get or set the Plastic hardening modulus.
          * - :py:attr:`~bulk`
            - Get or set the Bulk modulus, K.
          * - :py:attr:`~epf`
            - Get or set the Plastic failure strain.
          * - :py:attr:`~prf`
            - Get or set the Failure pressure (<= 0.0) (default = 0.0).
          * - :py:attr:`~rem`
            - Get or set the Element erosion option:
          * - :py:attr:`~trem`
            - Get or set the Dt for element removal:
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

    from mat_isotropic_elastic_failure import MatIsotropicElasticFailure

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

.. py:property:: g
   :type: Optional[float]


   
   Get or set the Shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the Yield stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: etan
   :type: Optional[float]


   
   Get or set the Plastic hardening modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the Bulk modulus, K.
















   ..
       !! processed by numpydoc !!

.. py:property:: epf
   :type: Optional[float]


   
   Get or set the Plastic failure strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: prf
   :type: Optional[float]


   
   Get or set the Failure pressure (<= 0.0) (default = 0.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: rem
   :type: Optional[float]


   
   Get or set the Element erosion option:
   EQ.0.0: failed element eroded after failure,
   NE.0.0: element is kept, no removal except by Dt below.
















   ..
       !! processed by numpydoc !!

.. py:property:: trem
   :type: Optional[float]


   
   Get or set the Dt for element removal:
   EQ.0.0: Dt is not considered (default),
   GT.0.0: element eroded if element time step size falls below Dt.
















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
   :value: 'ISOTROPIC_ELASTIC_FAILURE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





