





:class:`DampingPartStiffnessSet`
================================


.. py:class:: damping_part_stiffness_set.DampingPartStiffnessSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DAMPING_PART_STIFFNESS_SET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DampingPartStiffnessSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part Set ID, see *PART SET.
          * - :py:attr:`~coef`
            - Get or set the Rayleigh damping coefficient.  Two methods are now available:


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

    from damping_part_stiffness_set import DampingPartStiffnessSet

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part Set ID, see *PART SET.
















   ..
       !! processed by numpydoc !!

.. py:property:: coef
   :type: Optional[float]


   
   Get or set the Rayleigh damping coefficient.  Two methods are now available:
   LT.0.0: Rayleigh damping coefficient in units of time, set based on a given frequencyand applied uniformly to each element in the specified part or part set.This method is typically used for implicit dynamic analysis.See remarks below.
   EQ.0.0 : Inactive.
   GT.0.0 : Unitless damping coefficient for stiffness weighted damping.This non - classical method is typically used for explicit analyses as it does not require assembly of a stiffness matrix.Values between 0.01 and 0.25 are recommended.Higher values are strongly discouraged,and values less than 0.01 may have little effect.The damping coefficient is uniquely calculated internally for each element of the part ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DAMPING'


.. py:attribute:: subkeyword
   :value: 'PART_STIFFNESS_SET'






