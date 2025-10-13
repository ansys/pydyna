





:class:`IcfdBoundaryPrescribedTemp`
===================================


.. py:class:: icfd_boundary_prescribed_temp.IcfdBoundaryPrescribedTemp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_PRESCRIBED_TEMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryPrescribedTemp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID for a fluid surface.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe motion value versus time, see *DEFINE_ CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  See BIRTH below.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.  (default=1.0).
          * - :py:attr:`~death`
            - Get or set the Time imposed motion/constraint is removed.
          * - :py:attr:`~birth`
            - Get or set the Time imposed motion/constraint is activated starting from the initial abscissa value of the curve.


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

    from icfd_boundary_prescribed_temp import IcfdBoundaryPrescribedTemp

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID for a fluid surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe motion value versus time, see *DEFINE_ CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  See BIRTH below.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.  (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time imposed motion/constraint is removed.
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time imposed motion/constraint is activated starting from the initial abscissa value of the curve.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_TEMP'






