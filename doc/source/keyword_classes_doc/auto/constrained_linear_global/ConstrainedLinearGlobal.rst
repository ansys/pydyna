





:class:`ConstrainedLinearGlobal`
================================


.. py:class:: constrained_linear_global.ConstrainedLinearGlobal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_LINEAR_GLOBAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedLinearGlobal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~licd`
            - Get or set the Linear constraint definition ID. This ID can be used to identify a set to which this constraint is a member.
          * - :py:attr:`~nid`
            - Get or set the Node ID.
          * - :py:attr:`~dof`
            - Get or set the Degree of freedom in the local coordinate system;
          * - :py:attr:`~coef`
            - Get or set the Nonzero coefficient, Ck


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

    from constrained_linear_global import ConstrainedLinearGlobal

Property detail
---------------

.. py:property:: licd
   :type: Optional[int]


   
   Get or set the Linear constraint definition ID. This ID can be used to identify a set to which this constraint is a member.
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: int


   
   Get or set the Degree of freedom in the local coordinate system;
   EQ.1:displacement along global x-direction.
   EQ.2:displacement along global y-direction.
   EQ.3:displacement along global z-direction.
   EQ.4: global rotation about global x-axis.
   EQ.5: global rotation about global y-axis.
   EQ.6: global rotation about global z-axis.
   EQ.7:   Nodal electric voltage of piezoelectric material; see *MAT_ADD_PZELECTRIC.
   The voltage of the 1st node can only be defined as a linear combination of the voltage of other nodes, meaning all DOFs must be 7 for such an application.
















   ..
       !! processed by numpydoc !!

.. py:property:: coef
   :type: float


   
   Get or set the Nonzero coefficient, Ck
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'LINEAR_GLOBAL'






