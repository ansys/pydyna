





:class:`ControlFormingOutput`
=============================


.. py:class:: control_forming_output.ControlFormingOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the ID of a tooling kinematics curve, as defined by *DEFINE_CURVE and used by *BOUNDARY_PRESCRIBED_MOTION_RIGID.
          * - :py:attr:`~nout`
            - Get or set the Total number of D3PLOT outputs for the tooling kinematics curve, excluding the beginning and final time, see figures below.
          * - :py:attr:`~tbeg`
            - Get or set the Start time of the curve.
          * - :py:attr:`~tend`
            - Get or set the End time of the curve.
          * - :py:attr:`~y1_lcid`
            - Get or set the GT.0:    All four variables (Y1, Y2, Y3, Y4) are taken to be the distances from the punch home, where d3plot files will be output
          * - :py:attr:`~y2_lcid`
            - Get or set the GT.0:    The input is taken as the distance from the punch home, where a d3plot file will be output
          * - :py:attr:`~y3`
            - Get or set the Distances to tooling home, where D3PLOT files will be output.
          * - :py:attr:`~y4`
            - Get or set the Distances to tooling home, where D3PLOT files will be output.


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

    from control_forming_output import ControlFormingOutput

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the ID of a tooling kinematics curve, as defined by *DEFINE_CURVE and used by *BOUNDARY_PRESCRIBED_MOTION_RIGID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nout
   :type: Optional[int]


   
   Get or set the Total number of D3PLOT outputs for the tooling kinematics curve, excluding the beginning and final time, see figures below.
















   ..
       !! processed by numpydoc !!

.. py:property:: tbeg
   :type: Optional[float]


   
   Get or set the Start time of the curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: tend
   :type: Optional[float]


   
   Get or set the End time of the curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: y1_lcid
   :type: Optional[float]


   
   Get or set the GT.0:    All four variables (Y1, Y2, Y3, Y4) are taken to be the distances from the punch home, where d3plot files will be output
   LT.0:   The absolute value of Y1/LCID (must be an integer) is taken as a load curve ID (see *DEFINE_CURVE).
   Only the abscissas in the load curve, which are the distances to punch home, are used.
   These distances specify the states that are written to the d3plot files.  Ordinates of the curve are ignored.
   This case accommodates more states than is possible with the four variables Y1, Y2, Y3, Y4.  Furthermore, when Y1/LCID < 0, Y2, Y3, and Y4 are ignored.
   Available starting from Dev Revision 112604, the output will be skipped for any negative abscissa in the load curve.
   Note a curve with only negative abscissas is not allowed..
















   ..
       !! processed by numpydoc !!

.. py:property:: y2_lcid
   :type: Optional[float]


   
   Get or set the GT.0:    The input is taken as the distance from the punch home, where a d3plot file will be output
   LT.0:   The absolute value of Y2/CIDT (must be an integer) is taken as a load curve ID (see *DEFINE_CURVE).
   Only the abscissas in the load curve, which are the simulation times, are used.
   These times specify the states that are written to the d3plot files.  Ordinates of the curve are ignored.
   Note this time-dependent load curve will output additional d3plot files on top of the d3plot files already written in case Y1/LCID < 0 (if specified).
   Furthermore, when Y2/CIDT < 0, Y3 and Y4 are ignored.  See the example Using CIDT below.
















   ..
       !! processed by numpydoc !!

.. py:property:: y3
   :type: Optional[float]


   
   Get or set the Distances to tooling home, where D3PLOT files will be output.
















   ..
       !! processed by numpydoc !!

.. py:property:: y4
   :type: Optional[float]


   
   Get or set the Distances to tooling home, where D3PLOT files will be output.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_OUTPUT'






