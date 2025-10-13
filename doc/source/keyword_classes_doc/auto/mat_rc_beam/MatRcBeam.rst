





:class:`MatRcBeam`
==================


.. py:class:: mat_rc_beam.MatRcBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_RC_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatRcBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification.  A unique number or label must be specified.
          * - :py:attr:`~ro`
            - Get or set the Mass density.
          * - :py:attr:`~eunl`
            - Get or set the Initial unloading elastic modulus .
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~fc`
            - Get or set the Cylinder strength (stress units).
          * - :py:attr:`~ec1`
            - Get or set the Strain at which stress FC is reached.
          * - :py:attr:`~ec50`
            - Get or set the Strain at which the stress has dropped to 50% FC.
          * - :py:attr:`~resid`
            - Get or set the Residual strength factor
          * - :py:attr:`~ft`
            - Get or set the Maximum tensile stress.
          * - :py:attr:`~unitc`
            - Get or set the Factor to convert stress units to MPa.
          * - :py:attr:`~esoft`
            - Get or set the Slope of stress-strain curve post-cracking in tension.
          * - :py:attr:`~lchar`
            - Get or set the Characteristic length for strain-softening behavior.
          * - :py:attr:`~output`
            - Get or set the Output flag controlling what is written as  plastic strain
          * - :py:attr:`~fracr`
            - Get or set the Fraction of reinforcement (e.g. for 1% reinforcement FRACR=0.01).
          * - :py:attr:`~ymreinf`
            - Get or set the Young's Modulus of reinforcement.
          * - :py:attr:`~prreinf`
            - Get or set the Poisson's Ratio of reinforcement.
          * - :py:attr:`~syreinf`
            - Get or set the Yield stress of reinforcement.
          * - :py:attr:`~sureinf`
            - Get or set the Ultimate stress of reinforcement.
          * - :py:attr:`~eshr`
            - Get or set the Strain at which reinforcement begins to harden.
          * - :py:attr:`~eur`
            - Get or set the Strain at which reinforcement reaches ultimate stress.
          * - :py:attr:`~rreinf`
            - Get or set the Dimensionless Ramberg-Osgood parameter r. If zero, a default value r=4.0 will be used. If set to -1, parameters will be calculated from Kent & Park formulae
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

    from mat_rc_beam import MatRcBeam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification.  A unique number or label must be specified.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: eunl
   :type: Optional[float]


   
   Get or set the Initial unloading elastic modulus .
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: fc
   :type: Optional[float]


   
   Get or set the Cylinder strength (stress units).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec1
   :type: float


   
   Get or set the Strain at which stress FC is reached.
















   ..
       !! processed by numpydoc !!

.. py:property:: ec50
   :type: Optional[float]


   
   Get or set the Strain at which the stress has dropped to 50% FC.
















   ..
       !! processed by numpydoc !!

.. py:property:: resid
   :type: float


   
   Get or set the Residual strength factor
















   ..
       !! processed by numpydoc !!

.. py:property:: ft
   :type: Optional[float]


   
   Get or set the Maximum tensile stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: unitc
   :type: float


   
   Get or set the Factor to convert stress units to MPa.
















   ..
       !! processed by numpydoc !!

.. py:property:: esoft
   :type: Optional[float]


   
   Get or set the Slope of stress-strain curve post-cracking in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: lchar
   :type: Optional[float]


   
   Get or set the Characteristic length for strain-softening behavior.
















   ..
       !! processed by numpydoc !!

.. py:property:: output
   :type: float


   
   Get or set the Output flag controlling what is written as  plastic strain
   EQ.0.0: Curvature
   EQ.1.0:  High-tide  plastic strain in reinforcement
















   ..
       !! processed by numpydoc !!

.. py:property:: fracr
   :type: Optional[float]


   
   Get or set the Fraction of reinforcement (e.g. for 1% reinforcement FRACR=0.01).
















   ..
       !! processed by numpydoc !!

.. py:property:: ymreinf
   :type: Optional[float]


   
   Get or set the Young's Modulus of reinforcement.
















   ..
       !! processed by numpydoc !!

.. py:property:: prreinf
   :type: Optional[float]


   
   Get or set the Poisson's Ratio of reinforcement.
















   ..
       !! processed by numpydoc !!

.. py:property:: syreinf
   :type: Optional[float]


   
   Get or set the Yield stress of reinforcement.
















   ..
       !! processed by numpydoc !!

.. py:property:: sureinf
   :type: Optional[float]


   
   Get or set the Ultimate stress of reinforcement.
















   ..
       !! processed by numpydoc !!

.. py:property:: eshr
   :type: float


   
   Get or set the Strain at which reinforcement begins to harden.
















   ..
       !! processed by numpydoc !!

.. py:property:: eur
   :type: float


   
   Get or set the Strain at which reinforcement reaches ultimate stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: rreinf
   :type: float


   
   Get or set the Dimensionless Ramberg-Osgood parameter r. If zero, a default value r=4.0 will be used. If set to -1, parameters will be calculated from Kent & Park formulae
















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
   :value: 'RC_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





