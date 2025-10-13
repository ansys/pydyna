





:class:`Mat002Sunil`
====================


.. py:class:: mat_002_sunil.Mat002Sunil(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_002_SUNIL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: Mat002Sunil

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
          * - :py:attr:`~ea`
            - Get or set the Ea, Young's modulus in a-direction(1-1 direction).
          * - :py:attr:`~eb`
            - Get or set the Eb, Young's modulus in b-direction(2-2 direction).
          * - :py:attr:`~ec`
            - Get or set the Ec, Young's modulus in c-direction(3-3 direction).
          * - :py:attr:`~prba`
            - Get or set the Poisson's ratio, ba.
          * - :py:attr:`~prca`
            - Get or set the Poisson's ratio, ca.
          * - :py:attr:`~prcb`
            - Get or set the Poisson's ratio, cb.
          * - :py:attr:`~gab`
            - Get or set the Shear modulus, ab.
          * - :py:attr:`~gbc`
            - Get or set the Shear modulus, bc.
          * - :py:attr:`~gca`
            - Get or set the Shear modulus, ca.
          * - :py:attr:`~aopt`
            - Get or set the Material axes option.
          * - :py:attr:`~g`
            - Get or set the
          * - :py:attr:`~sigf`
            - Get or set the
          * - :py:attr:`~mfparm`
            - Get or set the
          * - :py:attr:`~xp`
            - Get or set the x-coordinates of point p for AOPT = 1.
          * - :py:attr:`~yp`
            - Get or set the y-coordinates of point p for AOPT = 1.
          * - :py:attr:`~zp`
            - Get or set the z-coordinates of point p for AOPT = 1.
          * - :py:attr:`~a1`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a2`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~a3`
            - Get or set the Component of vector a for AOPT = 2.
          * - :py:attr:`~macf`
            - Get or set the Material axes change flag for brick elements:
          * - :py:attr:`~v1`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v2`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~v3`
            - Get or set the Component of vector v for AOPT = 3.
          * - :py:attr:`~d1`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d2`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~d3`
            - Get or set the Component of vector d for AOPT = 2.
          * - :py:attr:`~beta`
            - Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
          * - :py:attr:`~ref`
            - Get or set the .
          * - :py:attr:`~t1fail`
            - Get or set the Fiber failure limit in tension (a-a, or 1-1 direction).
          * - :py:attr:`~c1fail`
            - Get or set the Fiber failure limit in compression (a-a, or 1-1 direction).
          * - :py:attr:`~t2fail`
            - Get or set the Fiber failure limit in tension (b-b, or 2-2 direction).
          * - :py:attr:`~c2fail`
            - Get or set the Fiber failure limit in compression (b-b, or 2-2 direction).
          * - :py:attr:`~t3fail`
            - Get or set the Fiber failure limit in tension (c-c, or 3-3 direction).
          * - :py:attr:`~c3fail`
            - Get or set the Fiber failure limit in compression (c-c or, 3-3 direction).
          * - :py:attr:`~s12fail`
            - Get or set the In-plane shear failure limit (a-b or, 1-2 direction).
          * - :py:attr:`~s23fail`
            - Get or set the Transverse shear failure limit (b-c or, 2-3 direction).
          * - :py:attr:`~s31fail`
            - Get or set the Transverse shear failure limit (c-a or, 3-1 direction).
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

    from mat_002_sunil import Mat002Sunil

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

.. py:property:: ea
   :type: Optional[float]


   
   Get or set the Ea, Young's modulus in a-direction(1-1 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: eb
   :type: Optional[float]


   
   Get or set the Eb, Young's modulus in b-direction(2-2 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: ec
   :type: Optional[float]


   
   Get or set the Ec, Young's modulus in c-direction(3-3 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: prba
   :type: Optional[float]


   
   Get or set the Poisson's ratio, ba.
















   ..
       !! processed by numpydoc !!

.. py:property:: prca
   :type: Optional[float]


   
   Get or set the Poisson's ratio, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: prcb
   :type: Optional[float]


   
   Get or set the Poisson's ratio, cb.
















   ..
       !! processed by numpydoc !!

.. py:property:: gab
   :type: Optional[float]


   
   Get or set the Shear modulus, ab.
















   ..
       !! processed by numpydoc !!

.. py:property:: gbc
   :type: Optional[float]


   
   Get or set the Shear modulus, bc.
















   ..
       !! processed by numpydoc !!

.. py:property:: gca
   :type: Optional[float]


   
   Get or set the Shear modulus, ca.
















   ..
       !! processed by numpydoc !!

.. py:property:: aopt
   :type: Optional[float]


   
   Get or set the Material axes option.
















   ..
       !! processed by numpydoc !!

.. py:property:: g
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: sigf
   :type: Optional[float]


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: mfparm
   :type: int


   
   Get or set the 
















   ..
       !! processed by numpydoc !!

.. py:property:: xp
   :type: Optional[float]


   
   Get or set the x-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: yp
   :type: Optional[float]


   
   Get or set the y-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: zp
   :type: Optional[float]


   
   Get or set the z-coordinates of point p for AOPT = 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: a1
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a2
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: a3
   :type: Optional[float]


   
   Get or set the Component of vector a for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: macf
   :type: int


   
   Get or set the Material axes change flag for brick elements:
   EQ.1:  No change, default,
   EQ.2:  switch material axes a and b,
   EQ.3:  switch material axes a and c,
   EQ.4:  switch material axes b and c.
















   ..
       !! processed by numpydoc !!

.. py:property:: v1
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v2
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: v3
   :type: Optional[float]


   
   Get or set the Component of vector v for AOPT = 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: d1
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d2
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: d3
   :type: Optional[float]


   
   Get or set the Component of vector d for AOPT = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
















   ..
       !! processed by numpydoc !!

.. py:property:: ref
   :type: Optional[float]


   
   Get or set the .
















   ..
       !! processed by numpydoc !!

.. py:property:: t1fail
   :type: Optional[float]


   
   Get or set the Fiber failure limit in tension (a-a, or 1-1 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: c1fail
   :type: Optional[float]


   
   Get or set the Fiber failure limit in compression (a-a, or 1-1 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: t2fail
   :type: Optional[float]


   
   Get or set the Fiber failure limit in tension (b-b, or 2-2 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: c2fail
   :type: Optional[float]


   
   Get or set the Fiber failure limit in compression (b-b, or 2-2 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: t3fail
   :type: Optional[float]


   
   Get or set the Fiber failure limit in tension (c-c, or 3-3 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: c3fail
   :type: Optional[float]


   
   Get or set the Fiber failure limit in compression (c-c or, 3-3 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: s12fail
   :type: Optional[float]


   
   Get or set the In-plane shear failure limit (a-b or, 1-2 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: s23fail
   :type: Optional[float]


   
   Get or set the Transverse shear failure limit (b-c or, 2-3 direction).
















   ..
       !! processed by numpydoc !!

.. py:property:: s31fail
   :type: Optional[float]


   
   Get or set the Transverse shear failure limit (c-a or, 3-1 direction).
















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
   :value: '002_SUNIL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





