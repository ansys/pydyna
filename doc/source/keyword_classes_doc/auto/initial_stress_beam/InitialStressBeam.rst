





:class:`InitialStressBeam`
==========================


.. py:class:: initial_stress_beam.InitialStressBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRESS_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Beam element ID.
          * - :py:attr:`~rule`
            - Get or set the Integration rule type number:
          * - :py:attr:`~npts`
            - Get or set the Number of integration points.  For the Belytschko-Schwer resultant beam element, NPTS=1.
          * - :py:attr:`~local`
            - Get or set the Coordinate system for stresses:
          * - :py:attr:`~large`
            - Get or set the Format size:
          * - :py:attr:`~nhisv`
            - Get or set the Number of additional history variables.  Only available for LARGE=1
          * - :py:attr:`~naxes`
            - Get or set the Number of variables giving beam local axes (0 or 12)
          * - :py:attr:`~f11`
            - Get or set the Axial force resultant along local beam axis 1.
          * - :py:attr:`~t11`
            - Get or set the Torsional moment resultant about local beam axis 1.
          * - :py:attr:`~m12`
            - Get or set the Moment resultant at node 1 about local beam axis 2.
          * - :py:attr:`~m13`
            - Get or set the Moment resultant at node 1 about local beam axis 3.
          * - :py:attr:`~m22`
            - Get or set the Moment resultant at node 2 about local beam axis 2.
          * - :py:attr:`~m23`
            - Get or set the Moment resultant at node 2 about local beam axis 3.
          * - :py:attr:`~parm`
            - Get or set the Generally not used.
          * - :py:attr:`~hisv1`
            - Get or set the Define the nth history variable
          * - :py:attr:`~hisv2`
            - Get or set the Define the nth history variable
          * - :py:attr:`~hisv3`
            - Get or set the Define the nth history variable
          * - :py:attr:`~sig11`
            - Get or set the Define the ij stress component.
          * - :py:attr:`~sig22`
            - Get or set the Define the ij stress component.
          * - :py:attr:`~sig33`
            - Get or set the Define the ij stress component.
          * - :py:attr:`~sig12`
            - Get or set the Define the ij stress component.
          * - :py:attr:`~sig23`
            - Get or set the Define the ij stress component.
          * - :py:attr:`~sig31`
            - Get or set the Define the ij stress component.
          * - :py:attr:`~eps`
            - Get or set the Effective plastic strain
          * - :py:attr:`~hisv4`
            - Get or set the Define the nth history variable.
          * - :py:attr:`~hisv5`
            - Get or set the Define the nth history variable
          * - :py:attr:`~hisv6`
            - Get or set the Define the nth history variable
          * - :py:attr:`~hisv7`
            - Get or set the Define the nth history variable
          * - :py:attr:`~hisv8`
            - Get or set the Define the nth history variable
          * - :py:attr:`~ax1`
            - Get or set the The nth local axes value.
          * - :py:attr:`~ax2`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax3`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax4`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax5`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax6`
            - Get or set the The nth local axes value.
          * - :py:attr:`~ax7`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax8`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax9`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax10`
            - Get or set the The nth local axes value
          * - :py:attr:`~ax11`
            - Get or set the The nth local axes value.
          * - :py:attr:`~ax12`
            - Get or set the The nth local axes value


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

    from initial_stress_beam import InitialStressBeam

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Beam element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: rule
   :type: int


   
   Get or set the Integration rule type number:
   EQ.1.0: 1x1 Gauss quadrature,
   EQ.2.0: 2x2 Gauss quadrature (default),
   EQ.3.0: 3x3 Gauss quadrature,
   EQ.4.0: 3x3 Lobatto quadrature,
   EQ.5.0: 4 x4 Gauss quadrature.
















   ..
       !! processed by numpydoc !!

.. py:property:: npts
   :type: int


   
   Get or set the Number of integration points.  For the Belytschko-Schwer resultant beam element, NPTS=1.
















   ..
       !! processed by numpydoc !!

.. py:property:: local
   :type: int


   
   Get or set the Coordinate system for stresses:
   EQ.0: Stress components are defined in the global coordinate system.
   EQ.1: stress components are defined in the local beam system. In the local system components SIG22, SIG33, and SIG23 are set to 0.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: large
   :type: int


   
   Get or set the Format size:
   EQ.0:   off,
   EQ.1:   on.  Each field is twice as long for higher precision.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: Optional[int]


   
   Get or set the Number of additional history variables.  Only available for LARGE=1
















   ..
       !! processed by numpydoc !!

.. py:property:: naxes
   :type: int


   
   Get or set the Number of variables giving beam local axes (0 or 12)
















   ..
       !! processed by numpydoc !!

.. py:property:: f11
   :type: float


   
   Get or set the Axial force resultant along local beam axis 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: t11
   :type: float


   
   Get or set the Torsional moment resultant about local beam axis 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: m12
   :type: float


   
   Get or set the Moment resultant at node 1 about local beam axis 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: m13
   :type: float


   
   Get or set the Moment resultant at node 1 about local beam axis 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: m22
   :type: float


   
   Get or set the Moment resultant at node 2 about local beam axis 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: m23
   :type: float


   
   Get or set the Moment resultant at node 2 about local beam axis 3.
















   ..
       !! processed by numpydoc !!

.. py:property:: parm
   :type: float


   
   Get or set the Generally not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv1
   :type: float


   
   Get or set the Define the nth history variable
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv2
   :type: float


   
   Get or set the Define the nth history variable
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv3
   :type: float


   
   Get or set the Define the nth history variable
















   ..
       !! processed by numpydoc !!

.. py:property:: sig11
   :type: float


   
   Get or set the Define the ij stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig22
   :type: float


   
   Get or set the Define the ij stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig33
   :type: float


   
   Get or set the Define the ij stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig12
   :type: float


   
   Get or set the Define the ij stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig23
   :type: float


   
   Get or set the Define the ij stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig31
   :type: float


   
   Get or set the Define the ij stress component.
















   ..
       !! processed by numpydoc !!

.. py:property:: eps
   :type: float


   
   Get or set the Effective plastic strain
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv4
   :type: float


   
   Get or set the Define the nth history variable.
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv5
   :type: float


   
   Get or set the Define the nth history variable
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv6
   :type: float


   
   Get or set the Define the nth history variable
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv7
   :type: float


   
   Get or set the Define the nth history variable
















   ..
       !! processed by numpydoc !!

.. py:property:: hisv8
   :type: float


   
   Get or set the Define the nth history variable
















   ..
       !! processed by numpydoc !!

.. py:property:: ax1
   :type: float


   
   Get or set the The nth local axes value.
















   ..
       !! processed by numpydoc !!

.. py:property:: ax2
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax3
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax4
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax5
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax6
   :type: float


   
   Get or set the The nth local axes value.
















   ..
       !! processed by numpydoc !!

.. py:property:: ax7
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax8
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax9
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax10
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!

.. py:property:: ax11
   :type: float


   
   Get or set the The nth local axes value.
















   ..
       !! processed by numpydoc !!

.. py:property:: ax12
   :type: float


   
   Get or set the The nth local axes value
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRESS_BEAM'






