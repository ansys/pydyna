





:class:`DefineFibers`
=====================


.. py:class:: define_fibers.DefineFibers(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_FIBERS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineFibers

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idf`
            - Get or set the ID of a fiber set to be defined, must be unique number.
          * - :py:attr:`~idp`
            - Get or set the Part ID of the matrix material associated with the fiber set.
          * - :py:attr:`~numf`
            - Get or set the Number of fiber orientations.
          * - :py:attr:`~n1`
            - Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
          * - :py:attr:`~n2`
            - Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
          * - :py:attr:`~efb`
            - Get or set the Effective stiffness of the fiber in its orientation, which typically equals to
          * - :py:attr:`~shr`
            - Get or set the Shear stiffness of the fiber:
          * - :py:attr:`~hrgls`
            - Get or set the Hourglass coefficient for stiffness type hourglass control. Default=1.0.
          * - :py:attr:`~alpha1`
            - Get or set the Initial orientation angles of the first, second and third fibers relative
          * - :py:attr:`~alpha2`
            - Get or set the Initial orientation angles of the first, second and third fibers relative
          * - :py:attr:`~alpha3`
            - Get or set the Initial orientation angles of the first, second and third fibers relative
          * - :py:attr:`~x1`
            - Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
          * - :py:attr:`~y1`
            - Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
          * - :py:attr:`~z1`
            - Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
          * - :py:attr:`~x2`
            - Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
          * - :py:attr:`~y2`
            - Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
          * - :py:attr:`~z2`
            - Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
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

    from define_fibers import DefineFibers

Property detail
---------------

.. py:property:: idf
   :type: Optional[int]


   
   Get or set the ID of a fiber set to be defined, must be unique number.
















   ..
       !! processed by numpydoc !!

.. py:property:: idp
   :type: Optional[int]


   
   Get or set the Part ID of the matrix material associated with the fiber set.
















   ..
       !! processed by numpydoc !!

.. py:property:: numf
   :type: Optional[int]


   
   Get or set the Number of fiber orientations.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Direction from Node 1 (N1) to Node 2 (N2) defines the reference fiber orientation.
















   ..
       !! processed by numpydoc !!

.. py:property:: efb
   :type: Optional[float]


   
   Get or set the Effective stiffness of the fiber in its orientation, which typically equals to
   Young’s Modulus times fiber cross sectional area fraction.
   Fiber cross sectional area fraction is typically between 0.25 and 0.5.
















   ..
       !! processed by numpydoc !!

.. py:property:: shr
   :type: Optional[float]


   
   Get or set the Shear stiffness of the fiber:
   GT.0:   shear stiffness,
   LT.0:   |SHR| is theload curve ID defining shear stiffness vs. shear strain.
















   ..
       !! processed by numpydoc !!

.. py:property:: hrgls
   :type: float


   
   Get or set the Hourglass coefficient for stiffness type hourglass control. Default=1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha1
   :type: Optional[float]


   
   Get or set the Initial orientation angles of the first, second and third fibers relative
   to reference fiber orientation defined by N1-N2, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha2
   :type: Optional[float]


   
   Get or set the Initial orientation angles of the first, second and third fibers relative
   to reference fiber orientation defined by N1-N2, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha3
   :type: Optional[float]


   
   Get or set the Initial orientation angles of the first, second and third fibers relative
   to reference fiber orientation defined by N1-N2, respectively.
















   ..
       !! processed by numpydoc !!

.. py:property:: x1
   :type: Optional[float]


   
   Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
   The Z1 and Z2 coordinate values must be defined close to the part.
   Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: y1
   :type: Optional[float]


   
   Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
   The Z1 and Z2 coordinate values must be defined close to the part.
   Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: z1
   :type: Optional[float]


   
   Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
   The Z1 and Z2 coordinate values must be defined close to the part.
   Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: x2
   :type: Optional[float]


   
   Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
   The Z1 and Z2 coordinate values must be defined close to the part.
   Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: y2
   :type: Optional[float]


   
   Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
   The Z1 and Z2 coordinate values must be defined close to the part.
   Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
















   ..
       !! processed by numpydoc !!

.. py:property:: z2
   :type: Optional[float]


   
   Get or set the If N1 and N2 are undefined, or zero, a third card is required to define the coordinates for N1 and N2.
   The Z1 and Z2 coordinate values must be defined close to the part.
   Based on the coordinate inputs, LS-DYNA will find the nearest nodes to define N1 and N2 from the model.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'FIBERS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





