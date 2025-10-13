





:class:`MatS15`
===============


.. py:class:: mat_s15.MatS15(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_S15 keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatS15

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A uniques number has to be used.
          * - :py:attr:`~lo`
            - Get or set the Initial muscle length.
          * - :py:attr:`~vmax`
            - Get or set the Maximum CE shortening velocity.
          * - :py:attr:`~sv`
            - Get or set the Scale factor for Vmax vs. active stat:
          * - :py:attr:`~a`
            - Get or set the Activation level vs. time function:
          * - :py:attr:`~fmax`
            - Get or set the Peak isometric force.
          * - :py:attr:`~tl`
            - Get or set the Active tension vs. length function:
          * - :py:attr:`~tv`
            - Get or set the Active tension vs. velocity function:
          * - :py:attr:`~fpe`
            - Get or set the Force vs. length function for parallel elastic element:
          * - :py:attr:`~lmax`
            - Get or set the Relative length when Fpe reaches Fmax. Required if Fpe=0 above.
          * - :py:attr:`~ksh`
            - Get or set the Constant governing the exponential rise of Fpe. Required if Fpe=0 above.
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

    from mat_s15 import MatS15

Property detail
---------------

.. py:property:: mid
   :type: int


   
   Get or set the Material identification. A uniques number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lo
   :type: Optional[float]


   
   Get or set the Initial muscle length.
















   ..
       !! processed by numpydoc !!

.. py:property:: vmax
   :type: Optional[float]


   
   Get or set the Maximum CE shortening velocity.
















   ..
       !! processed by numpydoc !!

.. py:property:: sv
   :type: float


   
   Get or set the Scale factor for Vmax vs. active stat:
   LT.0.0: absolute value gives load curve ID,
   GE.0.0: constant value of 1.0 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: a
   :type: Optional[float]


   
   Get or set the Activation level vs. time function:
   LT.0.0: absolute value gives load curve ID,
   GT.0.0: constant value of A is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: fmax
   :type: Optional[float]


   
   Get or set the Peak isometric force.
















   ..
       !! processed by numpydoc !!

.. py:property:: tl
   :type: float


   
   Get or set the Active tension vs. length function:
   LT.0.0: absolute value gives load curve ID,
   GT.0.0: constant value of 1.0 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: tv
   :type: float


   
   Get or set the Active tension vs. velocity function:
   LT.0.0: absolute value gives load curve ID,
   GT.0.0: constant value of 1.0 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: fpe
   :type: Optional[float]


   
   Get or set the Force vs. length function for parallel elastic element:
   LT.0.0: absolute value gives load curve ID,
   EQ.0.0: exponential function is used,
   GT.0.0: constant value of 0.0 is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: lmax
   :type: Optional[float]


   
   Get or set the Relative length when Fpe reaches Fmax. Required if Fpe=0 above.
















   ..
       !! processed by numpydoc !!

.. py:property:: ksh
   :type: Optional[float]


   
   Get or set the Constant governing the exponential rise of Fpe. Required if Fpe=0 above.
















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
   :value: 'S15'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





