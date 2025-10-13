





:class:`Mat100Damage`
=====================


.. py:class:: mat_100_damage.Mat100Damage(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_100_DAMAGE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat100Damage

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~sigy`
            - Get or set the GT.0:Initial yield stress.
          * - :py:attr:`~eh`
            - Get or set the Plastic hardening modulus, Eh.
          * - :py:attr:`~dt`
            - Get or set the Time step size for mass scaling, Delta t.
          * - :py:attr:`~tfail`
            - Get or set the Failure time if nonzero. If zero this option is ignored.
          * - :py:attr:`~efail`
            - Get or set the Effective plastic strain in weld material at failure.  The plastic strain must exceed the rupture strain (RS) at each integration point before deletion occurs.  See Card 3.
          * - :py:attr:`~nrr`
            - Get or set the Axial force resultant Nrrf or maximum axial stress at failure depending on the value of OPT (see below).
          * - :py:attr:`~nrs`
            - Get or set the Force resultant Nrsf or maximum shear stress Tf at failure depending on the value of OPT (see below).
          * - :py:attr:`~nrt`
            - Get or set the Force resultant NrtF at failure.
          * - :py:attr:`~mrr`
            - Get or set the Torsional moment resultant Mrrf at failure.
          * - :py:attr:`~mss`
            - Get or set the Moment resultant MssF at failure.
          * - :py:attr:`~mtt`
            - Get or set the Moment resultant MttF at failure.
          * - :py:attr:`~nf`
            - Get or set the Number of force vectors stored for filtering (default = 0). Default is recommended unless oscillatory resultant forces are observed in the time history databases. Even though these welds should not oscillate significantly, this option was added for consistency with the other spot weld options. NF affects the storage since it is necessary to store the resultant forces as history variables. When NF is nonzero, the resultants in the output databases are filtered.
          * - :py:attr:`~sigax`
            - Get or set the Maximum axial stress  rr F at failure. If zero, failure due to this component is not considered.
          * - :py:attr:`~sigtu`
            - Get or set the Maximum shear stress  F at failure. If zero, failure due to this component is not considered.
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

    from mat_100_damage import Mat100Damage

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

.. py:property:: e
   :type: Optional[float]


   
   Get or set the Young's modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: Optional[float]


   
   Get or set the Poisson's ratio.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigy
   :type: Optional[float]


   
   Get or set the GT.0:Initial yield stress.
   LT.0: A yield curve or table is assigned by |SIGY|.
















   ..
       !! processed by numpydoc !!

.. py:property:: eh
   :type: Optional[float]


   
   Get or set the Plastic hardening modulus, Eh.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt
   :type: Optional[float]


   
   Get or set the Time step size for mass scaling, Delta t.
















   ..
       !! processed by numpydoc !!

.. py:property:: tfail
   :type: Optional[float]


   
   Get or set the Failure time if nonzero. If zero this option is ignored.
















   ..
       !! processed by numpydoc !!

.. py:property:: efail
   :type: Optional[float]


   
   Get or set the Effective plastic strain in weld material at failure.  The plastic strain must exceed the rupture strain (RS) at each integration point before deletion occurs.  See Card 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrr
   :type: Optional[float]


   
   Get or set the Axial force resultant Nrrf or maximum axial stress at failure depending on the value of OPT (see below).
   If zero, failure due to this component is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrs
   :type: Optional[float]


   
   Get or set the Force resultant Nrsf or maximum shear stress Tf at failure depending on the value of OPT (see below).
   If zero, failure due to this component is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: nrt
   :type: Optional[float]


   
   Get or set the Force resultant NrtF at failure.
   If zero, failure due to this component is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mrr
   :type: Optional[float]


   
   Get or set the Torsional moment resultant Mrrf at failure.
   If zero, failure due to this component is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mss
   :type: Optional[float]


   
   Get or set the Moment resultant MssF at failure.
   If zero, failure due to this component is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: mtt
   :type: Optional[float]


   
   Get or set the Moment resultant MttF at failure.
   If zero, failure due to this component is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: nf
   :type: Optional[float]


   
   Get or set the Number of force vectors stored for filtering (default = 0). Default is recommended unless oscillatory resultant forces are observed in the time history databases. Even though these welds should not oscillate significantly, this option was added for consistency with the other spot weld options. NF affects the storage since it is necessary to store the resultant forces as history variables. When NF is nonzero, the resultants in the output databases are filtered.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigax
   :type: Optional[float]


   
   Get or set the Maximum axial stress  rr F at failure. If zero, failure due to this component is not considered.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigtu
   :type: Optional[float]


   
   Get or set the Maximum shear stress  F at failure. If zero, failure due to this component is not considered.
















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
   :value: '100_DAMAGE'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





