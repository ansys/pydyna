





:class:`MatNonlocal`
====================


.. py:class:: mat_nonlocal.MatNonlocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_NONLOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatNonlocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idnl`
            - Get or set the Nonlocal material input ID
          * - :py:attr:`~pid`
            - Get or set the Part ID for nonlocal material
          * - :py:attr:`~p`
            - Get or set the Exponent of weighting function. A typical value might be 8 depending somewhat on the choice of L. See equations in keyword user's manual.
          * - :py:attr:`~q`
            - Get or set the Exponent of weighting function. A typical value might be 2. See equations in keyword user's manual.
          * - :py:attr:`~l`
            - Get or set the Characteristic length.  This length should span a few elements. See equations in keyword user's manual.
          * - :py:attr:`~nfreq`
            - Get or set the Number of time steps between update of neighbours. The nearest neighbour search can add significant computational time so it is suggested that NFREQ be set to value of 10 to 100 depending on the problem. This parameter may be somewhat problem dependent.
          * - :py:attr:`~nhv`
            - Get or set the Define the number of history variables for nonlocal treatment..
          * - :py:attr:`~nl1`
            - Get or set the 1st history variable ID for nonlocal treatment
          * - :py:attr:`~nl2`
            - Get or set the 2nd history variable ID's for nonlocal treatment
          * - :py:attr:`~nl3`
            - Get or set the 3rd history variable ID's for nonlocal treatment
          * - :py:attr:`~nl4`
            - Get or set the 4th history variable ID's for nonlocal treatment
          * - :py:attr:`~nl5`
            - Get or set the 5th history variable ID's for nonlocal treatment
          * - :py:attr:`~nl6`
            - Get or set the 6th history variable ID's for nonlocal treatment
          * - :py:attr:`~nl7`
            - Get or set the 7th history variable ID's for nonlocal treatment
          * - :py:attr:`~nl8`
            - Get or set the 8th history variable ID's for nonlocal treatment
          * - :py:attr:`~xc1`
            - Get or set the X-coordinate of point on symmetry plane.
          * - :py:attr:`~yc1`
            - Get or set the Y-coordinate of point on symmetry plane.
          * - :py:attr:`~zc1`
            - Get or set the Z-coordinate of point on symmetry plane.
          * - :py:attr:`~xc2`
            - Get or set the X-coordinate of a point along the normal vector.
          * - :py:attr:`~yc2`
            - Get or set the Y-coordinate of a point along the normal vector.
          * - :py:attr:`~zc2`
            - Get or set the Z-coordinate of a point along the normal vector.
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

    from mat_nonlocal import MatNonlocal

Property detail
---------------

.. py:property:: idnl
   :type: Optional[int]


   
   Get or set the Nonlocal material input ID
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for nonlocal material
















   ..
       !! processed by numpydoc !!

.. py:property:: p
   :type: Optional[int]


   
   Get or set the Exponent of weighting function. A typical value might be 8 depending somewhat on the choice of L. See equations in keyword user's manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: q
   :type: Optional[int]


   
   Get or set the Exponent of weighting function. A typical value might be 2. See equations in keyword user's manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: l
   :type: Optional[float]


   
   Get or set the Characteristic length.  This length should span a few elements. See equations in keyword user's manual.
















   ..
       !! processed by numpydoc !!

.. py:property:: nfreq
   :type: Optional[int]


   
   Get or set the Number of time steps between update of neighbours. The nearest neighbour search can add significant computational time so it is suggested that NFREQ be set to value of 10 to 100 depending on the problem. This parameter may be somewhat problem dependent.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhv
   :type: Optional[int]


   
   Get or set the Define the number of history variables for nonlocal treatment..
















   ..
       !! processed by numpydoc !!

.. py:property:: nl1
   :type: Optional[int]


   
   Get or set the 1st history variable ID for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: nl2
   :type: Optional[int]


   
   Get or set the 2nd history variable ID's for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: nl3
   :type: Optional[int]


   
   Get or set the 3rd history variable ID's for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: nl4
   :type: Optional[int]


   
   Get or set the 4th history variable ID's for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: nl5
   :type: Optional[int]


   
   Get or set the 5th history variable ID's for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: nl6
   :type: Optional[int]


   
   Get or set the 6th history variable ID's for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: nl7
   :type: Optional[int]


   
   Get or set the 7th history variable ID's for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: nl8
   :type: Optional[int]


   
   Get or set the 8th history variable ID's for nonlocal treatment
















   ..
       !! processed by numpydoc !!

.. py:property:: xc1
   :type: Optional[float]


   
   Get or set the X-coordinate of point on symmetry plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc1
   :type: Optional[float]


   
   Get or set the Y-coordinate of point on symmetry plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc1
   :type: Optional[float]


   
   Get or set the Z-coordinate of point on symmetry plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: xc2
   :type: Optional[float]


   
   Get or set the X-coordinate of a point along the normal vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: yc2
   :type: Optional[float]


   
   Get or set the Y-coordinate of a point along the normal vector.
















   ..
       !! processed by numpydoc !!

.. py:property:: zc2
   :type: Optional[float]


   
   Get or set the Z-coordinate of a point along the normal vector.
















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
   :value: 'NONLOCAL'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





