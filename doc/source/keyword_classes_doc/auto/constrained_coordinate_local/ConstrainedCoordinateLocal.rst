





:class:`ConstrainedCoordinateLocal`
===================================


.. py:class:: constrained_coordinate_local.ConstrainedCoordinateLocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_COORDINATE_LOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedCoordinateLocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Identification number of a constraint.
          * - :py:attr:`~pid`
            - Get or set the Part ID or part set id of the part to be constrained.
          * - :py:attr:`~idir`
            - Get or set the Applicable degrees-of-freedom being constrained:
          * - :py:attr:`~x`
            - Get or set the x-coordinate coordinates of the location being constrained.
          * - :py:attr:`~y`
            - Get or set the y-coordinate coordinates of the location being constrained.
          * - :py:attr:`~z`
            - Get or set the z-coordinate coordinates of the location being constrained.
          * - :py:attr:`~cid`
            - Get or set the Local coordinate system ID.


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

    from constrained_coordinate_local import ConstrainedCoordinateLocal

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Identification number of a constraint.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID or part set id of the part to be constrained.
















   ..
       !! processed by numpydoc !!

.. py:property:: idir
   :type: int


   
   Get or set the Applicable degrees-of-freedom being constrained:
   EQ. 1: x translational degree-of-freedom,
   EQ. 2: y translational degree-of-freedom,
   EQ. 3: z translational degree-of-freedom.
















   ..
       !! processed by numpydoc !!

.. py:property:: x
   :type: Optional[float]


   
   Get or set the x-coordinate coordinates of the location being constrained.
















   ..
       !! processed by numpydoc !!

.. py:property:: y
   :type: Optional[float]


   
   Get or set the y-coordinate coordinates of the location being constrained.
















   ..
       !! processed by numpydoc !!

.. py:property:: z
   :type: Optional[float]


   
   Get or set the z-coordinate coordinates of the location being constrained.
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'COORDINATE_LOCAL'






