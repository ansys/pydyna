





:class:`ElementDiscreteLco`
===========================


.. py:class:: element_discrete_lco.ElementDiscreteLco(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_DISCRETE_LCO keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementDiscreteLco

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Element ID. A unique number has to be used.
          * - :py:attr:`~pid`
            - Get or set the Part ID, see *PART.
          * - :py:attr:`~n1`
            - Get or set the Nodal point 1.
          * - :py:attr:`~n2`
            - Get or set the Nodal point 2. If zero, the spring/damper connects node N1 to ground.
          * - :py:attr:`~vid`
            - Get or set the Orientation option: The orientation option should be used cautiously since forces, which are generated as the nodal points displace, are not orthogonal to rigid body rotation unless the nodes are coincident.. The type 6, 3D beam element, is recommended when orientation is required with the absolute value of the parameter SCOOR set to 2 or 3, since this option avoids rotational constraints.
          * - :py:attr:`~s`
            - Get or set the Scale factor on forces.
          * - :py:attr:`~pf`
            - Get or set the Print flag:
          * - :py:attr:`~offset`
            - Get or set the Initial offset. The initial offset is a displacement or rotation at time zero.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining the initial OFFSET as a function of time.  Positive offsets correspond to tensile forces, and, likewise negative offset result incompressive forces.
          * - :py:attr:`~lciddr`
            - Get or set the Load curve ID defining OFFSET as a function of time during the dynamic relaxation phase.


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

    from element_discrete_lco import ElementDiscreteLco

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Element ID. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Nodal point 1.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[int]


   
   Get or set the Nodal point 2. If zero, the spring/damper connects node N1 to ground.
















   ..
       !! processed by numpydoc !!

.. py:property:: vid
   :type: int


   
   Get or set the Orientation option: The orientation option should be used cautiously since forces, which are generated as the nodal points displace, are not orthogonal to rigid body rotation unless the nodes are coincident.. The type 6, 3D beam element, is recommended when orientation is required with the absolute value of the parameter SCOOR set to 2 or 3, since this option avoids rotational constraints.
   EQ.0: the spring/damper acts along the axis from node N1 to N2,
   NE.0: the spring/damper acts along the axis defined by the orientation vector, VID defined in the *DEFINE_SD_ORIENTATION section.
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: float


   
   Get or set the Scale factor on forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: pf
   :type: int


   
   Get or set the Print flag:
   EQ.0: forces are printed in DEFORC file, see *DATABASE_OPTION,
   EQ.1: forces are not printed in DEFORC file.
















   ..
       !! processed by numpydoc !!

.. py:property:: offset
   :type: float


   
   Get or set the Initial offset. The initial offset is a displacement or rotation at time zero.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining the initial OFFSET as a function of time.  Positive offsets correspond to tensile forces, and, likewise negative offset result incompressive forces.
















   ..
       !! processed by numpydoc !!

.. py:property:: lciddr
   :type: Optional[int]


   
   Get or set the Load curve ID defining OFFSET as a function of time during the dynamic relaxation phase.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'DISCRETE_LCO'






