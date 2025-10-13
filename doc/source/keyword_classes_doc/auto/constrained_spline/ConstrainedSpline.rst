





:class:`ConstrainedSpline`
==========================


.. py:class:: constrained_spline.ConstrainedSpline(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_SPLINE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedSpline

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~splid`
            - Get or set the spline constrained ID.
          * - :py:attr:`~dlratio`
            - Get or set the Ratio of bending to torsional stiffness for an elastic tubular beam which connects the independent degrees of freedom. The default value is set to 0.10.
          * - :py:attr:`~nid`
            - Get or set the Independent/dependent node ID. For explicit problems this node should not be a member of a rigid body, or elsewhere constrained in the input.
          * - :py:attr:`~dof`
            - Get or set the Degrees-of-dreedom. The list of dependent degrees-of-freedom consists of a number with up to six digits, with each digit representing a degree of dreedom. For example, the value 1356 indicates that degrees of freedom 1,3,5, and 6 are constrolled by the constrainet. The default is 123456. Digit:degree of freedom ID's:


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

    from constrained_spline import ConstrainedSpline

Property detail
---------------

.. py:property:: splid
   :type: Optional[int]


   
   Get or set the spline constrained ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dlratio
   :type: float


   
   Get or set the Ratio of bending to torsional stiffness for an elastic tubular beam which connects the independent degrees of freedom. The default value is set to 0.10.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Independent/dependent node ID. For explicit problems this node should not be a member of a rigid body, or elsewhere constrained in the input.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: Optional[int]


   
   Get or set the Degrees-of-dreedom. The list of dependent degrees-of-freedom consists of a number with up to six digits, with each digit representing a degree of dreedom. For example, the value 1356 indicates that degrees of freedom 1,3,5, and 6 are constrolled by the constrainet. The default is 123456. Digit:degree of freedom ID's:
   EQ:1 x
   EQ:2 Y
   EQ:3:z
   EQ:4:rotation about x axis
   EQ:5:rotation about y axis
   EQ:6:rotation about z axis
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'SPLINE'






