





:class:`MatLaminatedGlass`
==========================


.. py:class:: mat_laminated_glass.MatLaminatedGlass(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_LAMINATED_GLASS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatLaminatedGlass

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
          * - :py:attr:`~eg`
            - Get or set the Young's modulus for glass.
          * - :py:attr:`~prg`
            - Get or set the Poisson's ratio for glass.
          * - :py:attr:`~syg`
            - Get or set the Yield stress for glass.
          * - :py:attr:`~etg`
            - Get or set the Plastic hardening modulus for glass.
          * - :py:attr:`~efg`
            - Get or set the Plastic strain at failure for glass.
          * - :py:attr:`~ep`
            - Get or set the Young's modulus for polymer.
          * - :py:attr:`~prp`
            - Get or set the Poisson's ratio for polymer.
          * - :py:attr:`~syp`
            - Get or set the Yield stress for polymer.
          * - :py:attr:`~etp`
            - Get or set the Plastic hardening modulus for polymer.
          * - :py:attr:`~f1`
            - Get or set the Integration point material:
          * - :py:attr:`~f2`
            - Get or set the Integration point material:
          * - :py:attr:`~f3`
            - Get or set the Integration point material:
          * - :py:attr:`~f4`
            - Get or set the Integration point material:
          * - :py:attr:`~f5`
            - Get or set the Integration point material:
          * - :py:attr:`~f6`
            - Get or set the Integration point material:
          * - :py:attr:`~f7`
            - Get or set the Integration point material:
          * - :py:attr:`~f8`
            - Get or set the Integration point material:
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

    from mat_laminated_glass import MatLaminatedGlass

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

.. py:property:: eg
   :type: Optional[float]


   
   Get or set the Young's modulus for glass.
















   ..
       !! processed by numpydoc !!

.. py:property:: prg
   :type: Optional[float]


   
   Get or set the Poisson's ratio for glass.
















   ..
       !! processed by numpydoc !!

.. py:property:: syg
   :type: Optional[float]


   
   Get or set the Yield stress for glass.
















   ..
       !! processed by numpydoc !!

.. py:property:: etg
   :type: Optional[float]


   
   Get or set the Plastic hardening modulus for glass.
















   ..
       !! processed by numpydoc !!

.. py:property:: efg
   :type: Optional[float]


   
   Get or set the Plastic strain at failure for glass.
















   ..
       !! processed by numpydoc !!

.. py:property:: ep
   :type: Optional[float]


   
   Get or set the Young's modulus for polymer.
















   ..
       !! processed by numpydoc !!

.. py:property:: prp
   :type: Optional[float]


   
   Get or set the Poisson's ratio for polymer.
















   ..
       !! processed by numpydoc !!

.. py:property:: syp
   :type: Optional[float]


   
   Get or set the Yield stress for polymer.
















   ..
       !! processed by numpydoc !!

.. py:property:: etp
   :type: Optional[float]


   
   Get or set the Plastic hardening modulus for polymer.
















   ..
       !! processed by numpydoc !!

.. py:property:: f1
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass (default),
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: f2
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass,
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: f3
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass,
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: f4
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass,
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: f5
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass,
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: f6
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass,
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: f7
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass,
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















   ..
       !! processed by numpydoc !!

.. py:property:: f8
   :type: float


   
   Get or set the Integration point material:
   EQ.0.0: glass,
   EQ.1.0: polymer.
   A user-defined integration rule must be specified, see *INTEGRATION_SHELL.
















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
   :value: 'LAMINATED_GLASS'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





