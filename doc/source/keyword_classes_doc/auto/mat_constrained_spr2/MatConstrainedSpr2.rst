





:class:`MatConstrainedSpr2`
===========================


.. py:class:: mat_constrained_spr2.MatConstrainedSpr2(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_CONSTRAINED_SPR2 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatConstrainedSpr2

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~fn`
            - Get or set the Rivet strength in tension (pull-out).
          * - :py:attr:`~ft`
            - Get or set the Rivet strength in pure shear.
          * - :py:attr:`~dn`
            - Get or set the Failure displacement in normal direction.
          * - :py:attr:`~dt`
            - Get or set the Failure displacement in tangential direction.
          * - :py:attr:`~xin`
            - Get or set the Fraction of failure displacement at maximum normal force.
          * - :py:attr:`~xit`
            - Get or set the Fraction of failure displacement at maximum tangential force.
          * - :py:attr:`~alpha1`
            - Get or set the Dimensionless parameter scaling the effective displacement.
          * - :py:attr:`~alpha2`
            - Get or set the Dimensionless parameter scaling the effective displacement.
          * - :py:attr:`~alpha3`
            - Get or set the Dimensionless parameter scaling the effective displacement.
          * - :py:attr:`~expn`
            - Get or set the Exponent value for load function in normal direction.
          * - :py:attr:`~expt`
            - Get or set the Exponent value for load function in tangential direction.
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

    from mat_constrained_spr2 import MatConstrainedSpr2

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: fn
   :type: Optional[float]


   
   Get or set the Rivet strength in tension (pull-out).
















   ..
       !! processed by numpydoc !!

.. py:property:: ft
   :type: Optional[float]


   
   Get or set the Rivet strength in pure shear.
















   ..
       !! processed by numpydoc !!

.. py:property:: dn
   :type: Optional[float]


   
   Get or set the Failure displacement in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Failure displacement in tangential direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: xin
   :type: Optional[float]


   
   Get or set the Fraction of failure displacement at maximum normal force.
















   ..
       !! processed by numpydoc !!

.. py:property:: xit
   :type: Optional[float]


   
   Get or set the Fraction of failure displacement at maximum tangential force.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Dimensionless parameter scaling the effective displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Dimensionless parameter scaling the effective displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the Dimensionless parameter scaling the effective displacement.
















   ..
       !! processed by numpydoc !!

.. py:property:: expn
   :type: float


   
   Get or set the Exponent value for load function in normal direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: expt
   :type: float


   
   Get or set the Exponent value for load function in tangential direction.
















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
   :value: 'CONSTRAINED_SPR2'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





