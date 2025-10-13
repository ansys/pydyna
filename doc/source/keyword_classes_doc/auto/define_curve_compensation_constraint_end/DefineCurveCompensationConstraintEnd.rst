





:class:`DefineCurveCompensationConstraintEnd`
=============================================


.. py:class:: define_curve_compensation_constraint_end.DefineCurveCompensationConstraintEnd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_COMPENSATION_CONSTRAINT_END keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveCompensationConstraintEnd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~crvid`
            - Get or set the Curve ID; must be unique, with the same begin and end point coordinates.
          * - :py:attr:`~in_out`
            - Get or set the Flag to indicate local area to be compensated:
          * - :py:attr:`~x`
            - Get or set the X-coordinate of a point on the curve.
          * - :py:attr:`~y`
            - Get or set the Y-coordinate of a point on the curve
          * - :py:attr:`~z`
            - Get or set the Z-coordinate of a point on the curve.
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

    from define_curve_compensation_constraint_end import DefineCurveCompensationConstraintEnd

Property detail
---------------

.. py:property:: crvid
   :type: Optional[int]


   
   Get or set the Curve ID; must be unique, with the same begin and end point coordinates.
















   ..
       !! processed by numpydoc !!

.. py:property:: in_out
   :type: Optional[int]


   
   Get or set the Flag to indicate local area to be compensated:
   EQ.1: Compensate area includes enclosed curve under keyword 'BEGIN' and transition area between the two curves; no changes will be made to the area outside the curve under keyword , 'END'
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: float


   
   Get or set the X-coordinate of a point on the curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: float


   
   Get or set the Y-coordinate of a point on the curve
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: float


   
   Get or set the Z-coordinate of a point on the curve.
















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
   :value: 'CURVE_COMPENSATION_CONSTRAINT_END'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





