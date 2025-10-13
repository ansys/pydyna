





:class:`LoadErodingPartSet`
===========================


.. py:class:: load_eroding_part_set.LoadErodingPartSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_ERODING_PART_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadErodingPartSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID number.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining pressure as a function of time, see *DEFINE_CURVE.
          * - :py:attr:`~sf`
            - Get or set the Scale factor.
          * - :py:attr:`~at`
            - Get or set the Arrival time.
          * - :py:attr:`~psid`
            - Get or set the Part set ID, see *SET_PART..
          * - :py:attr:`~boxid`
            - Get or set the Box ID, see *DEFINE_BOX.
          * - :py:attr:`~mem`
            - Get or set the Extra memory, in percent, to be allocated above the initial memory for storing the new load segments exposed by the erosion.
          * - :py:attr:`~alpha`
            - Get or set the The maximum angle (in degrees) permitted between the normal of a segment at its centroid and the average normal at its nodes. This angle is used to eliminate interior segments.
          * - :py:attr:`~iflag`
            - Get or set the Flag for choosing a subset of the exposed surface that is oriented towards a blast or other loading source. The vector from the center of the element to the source location must be within an angle of BETA of the surface normal. If IFLAG>0, then the subset is chosen, otherwise if  IFLAG=0, the entire surface is loaded.
          * - :py:attr:`~x`
            - Get or set the Optional source location.
          * - :py:attr:`~y`
            - Get or set the Optional source location.
          * - :py:attr:`~z`
            - Get or set the Optional source location.
          * - :py:attr:`~beta`
            - Get or set the Maximum permitted angle (in degrees) between the surface normal and the vector to the source. The exposed segment is not loaded if the calculated angle is greater than BETA.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from load_eroding_part_set import LoadErodingPartSet

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID number.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining pressure as a function of time, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor.
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: float


   
   Get or set the Arrival time.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID, see *SET_PART..
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the Box ID, see *DEFINE_BOX.
















   ..
       !! processed by numpydoc !!

.. py:property:: mem
   :type: int


   
   Get or set the Extra memory, in percent, to be allocated above the initial memory for storing the new load segments exposed by the erosion.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the The maximum angle (in degrees) permitted between the normal of a segment at its centroid and the average normal at its nodes. This angle is used to eliminate interior segments.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: int


   
   Get or set the Flag for choosing a subset of the exposed surface that is oriented towards a blast or other loading source. The vector from the center of the element to the source location must be within an angle of BETA of the surface normal. If IFLAG>0, then the subset is chosen, otherwise if  IFLAG=0, the entire surface is loaded.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the Optional source location.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the Optional source location.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the Optional source location.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Maximum permitted angle (in degrees) between the surface normal and the vector to the source. The exposed segment is not loaded if the calculated angle is greater than BETA.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'ERODING_PART_SET'






