





:class:`DefineCurveFeedback`
============================


.. py:class:: define_curve_feedback.DefineCurveFeedback(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_CURVE_FEEDBACK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineCurveFeedback

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~lcid`
            - Get or set the ID number for load curve to be scaled.
          * - :py:attr:`~pid`
            - Get or set the Active part ID for load curve control
          * - :py:attr:`~boxid`
            - Get or set the Box ID. Elements of specified part ID contained in box are checked. If the box ID is set to zero then all elements of the active part are checked.
          * - :py:attr:`~fldid`
            - Get or set the Load curve ID which defines the flow limit. If the product of FSL and the ordinate value of the maximum principal strain is exceeded the scale factor for flow, SF, is active.
          * - :py:attr:`~fsl`
            - Get or set the If the strain ratio, epsilon-major-workpiece to epsilon-major-fld, is exceeded the scale factor for flow, SF, is active.
          * - :py:attr:`~tsl`
            - Get or set the Thickness strain limit. If the through thickness strain is exceeded the scale factor for thickening, ST, is active.
          * - :py:attr:`~sff`
            - Get or set the Scale factor for the flow limit diagram, SF (default=1.0).
          * - :py:attr:`~sft`
            - Get or set the Scale factor for thickening, ST (default=1.0).
          * - :py:attr:`~bias`
            - Get or set the Bias for combined flow and thickening, S, -1.0 <= S <= 1.0.
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

    from define_curve_feedback import DefineCurveFeedback

Property detail
---------------

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the ID number for load curve to be scaled.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Active part ID for load curve control
















   ..
       !! processed by numpydoc !!

.. py:property:: boxid
   :type: int


   
   Get or set the Box ID. Elements of specified part ID contained in box are checked. If the box ID is set to zero then all elements of the active part are checked.
















   ..
       !! processed by numpydoc !!

.. py:property:: fldid
   :type: Optional[int]


   
   Get or set the Load curve ID which defines the flow limit. If the product of FSL and the ordinate value of the maximum principal strain is exceeded the scale factor for flow, SF, is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: fsl
   :type: Optional[float]


   
   Get or set the If the strain ratio, epsilon-major-workpiece to epsilon-major-fld, is exceeded the scale factor for flow, SF, is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: tsl
   :type: Optional[float]


   
   Get or set the Thickness strain limit. If the through thickness strain is exceeded the scale factor for thickening, ST, is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: sff
   :type: float


   
   Get or set the Scale factor for the flow limit diagram, SF (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: sft
   :type: float


   
   Get or set the Scale factor for thickening, ST (default=1.0).
















   ..
       !! processed by numpydoc !!

.. py:property:: bias
   :type: float


   
   Get or set the Bias for combined flow and thickening, S, -1.0 <= S <= 1.0.
















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
   :value: 'CURVE_FEEDBACK'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





