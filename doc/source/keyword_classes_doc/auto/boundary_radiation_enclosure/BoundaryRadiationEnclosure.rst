





:class:`BoundaryRadiationEnclosure`
===================================


.. py:class:: boundary_radiation_enclosure.BoundaryRadiationEnclosure(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_RADIATION_ENCLOSURE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryRadiationEnclosure

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~brencid`
            - Get or set the Boundary radiation ID for this enclosure
          * - :py:attr:`~encname`
            - Get or set the Name of enclosure, used for output purposes
          * - :py:attr:`~calopt`
            - Get or set the Calculation option:
          * - :py:attr:`~outopt`
            - Get or set the Output option:
          * - :py:attr:`~conopt`
            - Get or set the Control option:
          * - :py:attr:`~smflag`
            - Get or set the View factor matrix smoothing flag:
          * - :py:attr:`~smmaxi`
            - Get or set the Maximum number of iterations for view factor matrix smoothing (default = 500)
          * - :py:attr:`~smabst`
            - Get or set the Absolute convergence tolerance for view factor matrix smoothing (default = 10-10)
          * - :py:attr:`~smrelt`
            - Get or set the Relative convergence tolerance for view factor matrix smoothing (default = 10-6)
          * - :py:attr:`~stype`
            - Get or set the Solver type:
          * - :py:attr:`~slmaxi`
            - Get or set the Maximum number of iterations for radiosity solver (default = 500)
          * - :py:attr:`~slabst`
            - Get or set the Absolute convergence tolerance for radiosity solver (default is 10-10)
          * - :py:attr:`~slrelt`
            - Get or set the Relative convergence tolerance for radiosity solver (default = 10-6)
          * - :py:attr:`~slmlev`
            - Get or set the Radiosity solver message level:
          * - :py:attr:`~ssid`
            - Get or set the SSID specifies the ID for a set of segments that comprise a portion of, or possibly, the entire enclosure. See *SET_‌SEGMENT.
          * - :py:attr:`~nint`
            - Get or set the Number of integration points for view factor calculation, 1 ≤ NINT ≤ 10
          * - :py:attr:`~block`
            - Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces:
          * - :py:attr:`~selcid`
            - Get or set the Load curve ID for surface emissivity (see *DEFINE_‌CURVE):
          * - :py:attr:`~semult`
            - Get or set the Curve multiplier for surface emissivity; see *DEFINE_‌CURVE
          * - :py:attr:`~loc`
            - Get or set the Application of surface for thermal shell elements (see THSHEL in the *CONTROL_‌SHELL input):


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

    from boundary_radiation_enclosure import BoundaryRadiationEnclosure

Property detail
---------------

.. py:property:: brencid
   :type: Optional[int]


   
   Get or set the Boundary radiation ID for this enclosure
















   ..
       !! processed by numpydoc !!

.. py:property:: encname
   :type: Optional[str]


   
   Get or set the Name of enclosure, used for output purposes
















   ..
       !! processed by numpydoc !!

.. py:property:: calopt
   :type: int


   
   Get or set the Calculation option:
   EQ.0:   view factors
















   ..
       !! processed by numpydoc !!

.. py:property:: outopt
   :type: int


   
   Get or set the Output option:
   EQ.0:   no output
   EQ.1 : output in LSDA format
















   ..
       !! processed by numpydoc !!

.. py:property:: conopt
   :type: int


   
   Get or set the Control option:
   EQ.0:   calculate view factors matrix and preform thermal analysis
















   ..
       !! processed by numpydoc !!

.. py:property:: smflag
   :type: int


   
   Get or set the View factor matrix smoothing flag:
   EQ.0:   no smoothing
   EQ.1 : smoothing
















   ..
       !! processed by numpydoc !!

.. py:property:: smmaxi
   :type: int


   
   Get or set the Maximum number of iterations for view factor matrix smoothing (default = 500)
















   ..
       !! processed by numpydoc !!

.. py:property:: smabst
   :type: float


   
   Get or set the Absolute convergence tolerance for view factor matrix smoothing (default = 10-10)
















   ..
       !! processed by numpydoc !!

.. py:property:: smrelt
   :type: float


   
   Get or set the Relative convergence tolerance for view factor matrix smoothing (default = 10-6)
















   ..
       !! processed by numpydoc !!

.. py:property:: stype
   :type: int


   
   Get or set the Solver type:
   EQ.0:   reverse conjugated gradient
















   ..
       !! processed by numpydoc !!

.. py:property:: slmaxi
   :type: int


   
   Get or set the Maximum number of iterations for radiosity solver (default = 500)
















   ..
       !! processed by numpydoc !!

.. py:property:: slabst
   :type: float


   
   Get or set the Absolute convergence tolerance for radiosity solver (default is 10-10)
















   ..
       !! processed by numpydoc !!

.. py:property:: slrelt
   :type: float


   
   Get or set the Relative convergence tolerance for radiosity solver (default = 10-6)
















   ..
       !! processed by numpydoc !!

.. py:property:: slmlev
   :type: int


   
   Get or set the Radiosity solver message level:
   EQ.0:   no output
   EQ.1 : debug output level I
   EQ.2 : debug output level II
   EQ.3 : debug output level III
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the SSID specifies the ID for a set of segments that comprise a portion of, or possibly, the entire enclosure. See *SET_‌SEGMENT.
















   ..
       !! processed by numpydoc !!

.. py:property:: nint
   :type: Optional[int]


   
   Get or set the Number of integration points for view factor calculation, 1 ≤ NINT ≤ 10
   EQ.0:   LS - DYNA determines the number of integration points based on the segment size and separation distance.
















   ..
       !! processed by numpydoc !!

.. py:property:: block
   :type: int


   
   Get or set the Flag indicating if this surface blocks the view between any other 2 surfaces:
   EQ.0:   no blocking(default)
   EQ.1 : blocking
















   ..
       !! processed by numpydoc !!

.. py:property:: selcid
   :type: int


   
   Get or set the Load curve ID for surface emissivity (see *DEFINE_‌CURVE):
   GT.0:   surface emissivity as a function of time
   EQ.0 : use constant multiplier value, SEMULT
   LT.0 : surface emissivity as a function of temperature.The value of –SELCID must be an integer,and it is interpreted as a load curve ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: semult
   :type: float


   
   Get or set the Curve multiplier for surface emissivity; see *DEFINE_‌CURVE
















   ..
       !! processed by numpydoc !!

.. py:property:: loc
   :type: int


   
   Get or set the Application of surface for thermal shell elements (see THSHEL in the *CONTROL_‌SHELL input):
   EQ. - 1:        lower surface of thermal shell element
   EQ.0 : middle surface of thermal shell element
   EQ.1 : upper surface of thermal shell element
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'RADIATION_ENCLOSURE'






