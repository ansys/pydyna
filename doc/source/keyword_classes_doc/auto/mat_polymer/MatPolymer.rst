





:class:`MatPolymer`
===================


.. py:class:: mat_polymer.MatPolymer(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_POLYMER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatPolymer

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
          * - :py:attr:`~e`
            - Get or set the Young's modulus.
          * - :py:attr:`~pr`
            - Get or set the Poisson's ratio.
          * - :py:attr:`~gamma0`
            - Get or set the Pre-exponential factor.
          * - :py:attr:`~dg`
            - Get or set the Energy barrier to flow.
          * - :py:attr:`~sc`
            - Get or set the Shear resistance in compression.
          * - :py:attr:`~st`
            - Get or set the Shear resistance in tension.
          * - :py:attr:`~temp`
            - Get or set the Absolute temperature.
          * - :py:attr:`~k`
            - Get or set the Boltzmann constant.
          * - :py:attr:`~cr`
            - Get or set the Product.
          * - :py:attr:`~n`
            - Get or set the Number of  rigid links' between entanglements.
          * - :py:attr:`~c`
            - Get or set the Relaxation factor.
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

    from mat_polymer import MatPolymer

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

.. py:property:: gamma0
   :type: Optional[float]


   
   Get or set the Pre-exponential factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: dg
   :type: Optional[float]


   
   Get or set the Energy barrier to flow.
















   ..
       !! processed by numpydoc !!

.. py:property:: sc
   :type: Optional[float]


   
   Get or set the Shear resistance in compression.
















   ..
       !! processed by numpydoc !!

.. py:property:: st
   :type: Optional[float]


   
   Get or set the Shear resistance in tension.
















   ..
       !! processed by numpydoc !!

.. py:property:: temp
   :type: Optional[float]


   
   Get or set the Absolute temperature.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: Optional[float]


   
   Get or set the Boltzmann constant.
















   ..
       !! processed by numpydoc !!

.. py:property:: cr
   :type: Optional[float]


   
   Get or set the Product.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: Optional[float]


   
   Get or set the Number of  rigid links' between entanglements.
















   ..
       !! processed by numpydoc !!

.. py:property:: c
   :type: Optional[float]


   
   Get or set the Relaxation factor.
















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
   :value: 'POLYMER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





