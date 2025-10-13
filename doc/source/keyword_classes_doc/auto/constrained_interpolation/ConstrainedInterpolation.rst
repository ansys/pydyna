





:class:`ConstrainedInterpolation`
=================================


.. py:class:: constrained_interpolation.ConstrainedInterpolation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_INTERPOLATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedInterpolation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~icid`
            - Get or set the Interpolation constraint ID.
          * - :py:attr:`~dnid`
            - Get or set the Dependent node ID. This node should not be a member of a rigid body, or elsewhere constrained in the input.
          * - :py:attr:`~ddof`
            - Get or set the Dependent degrees-of-freedom. The list of dependent DOF consists of a number with up to six digits, with each digit representing a degree of freedom, e.g., the value 1356 indicates that degrees of freedom 1, 3, 5, and 6 are controlled by the RBE3 constraint. Default=123456.
          * - :py:attr:`~cidd`
            - Get or set the Local coordinate system ID of LOCAL option is active. If blank the global coordinate system is assumed.
          * - :py:attr:`~ityp`
            - Get or set the Specifies the meaning of INID.
          * - :py:attr:`~idnsw`
            - Get or set the Switch for controlling the explicit solution  when an independent (or dependent) node is deleted.
          * - :py:attr:`~fgm`
            - Get or set the Flag for special treatment of this constraint for implicit problems only:
          * - :py:attr:`~inid`
            - Get or set the Independent node ID or node set ID.
          * - :py:attr:`~idof`
            - Get or set the Independent degrees-of-freedom using the same form as DDOF above.
          * - :py:attr:`~twghtx`
            - Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the x-translational component. It is normally sufficient to define only TWGHTX even if its degree-of-freedom is inactive since the other factors are set equal to this input value as the default. There is no requirement on the values that are chosen as the weighting factors, i.e., that they sum to unity. The default value for the weighting factor is unity.
          * - :py:attr:`~twghty`
            - Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the y-translational component.
          * - :py:attr:`~twghtz`
            - Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the z-translational component.
          * - :py:attr:`~rwghtx`
            - Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the x-rotational component.
          * - :py:attr:`~rwghty`
            - Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the y-rotational component.
          * - :py:attr:`~rwghtz`
            - Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the z-rotational component.


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

    from constrained_interpolation import ConstrainedInterpolation

Property detail
---------------

.. py:property:: icid
   :type: Optional[int]


   
   Get or set the Interpolation constraint ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dnid
   :type: int


   
   Get or set the Dependent node ID. This node should not be a member of a rigid body, or elsewhere constrained in the input.
















   ..
       !! processed by numpydoc !!

.. py:property:: ddof
   :type: int


   
   Get or set the Dependent degrees-of-freedom. The list of dependent DOF consists of a number with up to six digits, with each digit representing a degree of freedom, e.g., the value 1356 indicates that degrees of freedom 1, 3, 5, and 6 are controlled by the RBE3 constraint. Default=123456.
   Degree of freedom IDs:
   EQ.1: x,
   EQ.2: y,
   EQ.3: z,
   EQ.4: rotation about x-axis,
   EQ.5: rotation about y-axis,
   EQ.6: rotation about z-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: cidd
   :type: Optional[int]


   
   Get or set the Local coordinate system ID of LOCAL option is active. If blank the global coordinate system is assumed.
















   ..
       !! processed by numpydoc !!

.. py:property:: ityp
   :type: int


   
   Get or set the Specifies the meaning of INID.
   EQ.0: INID is a node ID
   EQ.1: INID is a node set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: idnsw
   :type: int


   
   Get or set the Switch for controlling the explicit solution  when an independent (or dependent) node is deleted.
   EQ.0:   default to option 1.
   EQ.1:   terminate the explicit analysis when an independent node or the dependent node is deleted.
   EQ.2:   continue the explicit analysis with the constraints unchanged. .
















   ..
       !! processed by numpydoc !!

.. py:property:: fgm
   :type: int


   
   Get or set the Flag for special treatment of this constraint for implicit problems only:
   EQ.0:   use standard constraint processing for implicit.
   EQ.1 : use special processing for this constraint for implicit only; see Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: inid
   :type: int


   
   Get or set the Independent node ID or node set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: idof
   :type: int


   
   Get or set the Independent degrees-of-freedom using the same form as DDOF above.
















   ..
       !! processed by numpydoc !!

.. py:property:: twghtx
   :type: float


   
   Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the x-translational component. It is normally sufficient to define only TWGHTX even if its degree-of-freedom is inactive since the other factors are set equal to this input value as the default. There is no requirement on the values that are chosen as the weighting factors, i.e., that they sum to unity. The default value for the weighting factor is unity.
















   ..
       !! processed by numpydoc !!

.. py:property:: twghty
   :type: float


   
   Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the y-translational component.
















   ..
       !! processed by numpydoc !!

.. py:property:: twghtz
   :type: float


   
   Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the z-translational component.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwghtx
   :type: float


   
   Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the x-rotational component.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwghty
   :type: float


   
   Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the y-rotational component.
















   ..
       !! processed by numpydoc !!

.. py:property:: rwghtz
   :type: float


   
   Get or set the Weighting factor for node INID with active degrees-of-freedom IDOF.  This weight scales the z-rotational component.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'INTERPOLATION'






