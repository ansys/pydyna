





:class:`MatRambergOsgood`
=========================


.. py:class:: mat_ramberg_osgood.MatRambergOsgood(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_RAMBERG_OSGOOD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatRambergOsgood

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
          * - :py:attr:`~gamy`
            - Get or set the Reference shear strain (gamma-y).
          * - :py:attr:`~tauy`
            - Get or set the Reference shear stress (tau-y).
          * - :py:attr:`~alpha`
            - Get or set the Stress coefficient (alpha).
          * - :py:attr:`~r`
            - Get or set the Stress exponent (r).
          * - :py:attr:`~bulk`
            - Get or set the Elastic bulk modulus.
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

    from mat_ramberg_osgood import MatRambergOsgood

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

.. py:property:: gamy
   :type: Optional[float]


   
   Get or set the Reference shear strain (gamma-y).
















   ..
       !! processed by numpydoc !!

.. py:property:: tauy
   :type: Optional[float]


   
   Get or set the Reference shear stress (tau-y).
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: Optional[float]


   
   Get or set the Stress coefficient (alpha).
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Stress exponent (r).
















   ..
       !! processed by numpydoc !!

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the Elastic bulk modulus.
















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
   :value: 'RAMBERG_OSGOOD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





