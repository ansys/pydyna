





:class:`ConstrainedLinearLocal`
===============================


.. py:class:: constrained_linear_local.ConstrainedLinearLocal(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_LINEAR_LOCAL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedLinearLocal

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the ID for linear constraint definition
          * - :py:attr:`~nid`
            - Get or set the Node ID
          * - :py:attr:`~dof`
            - Get or set the Degrees of freedom in the local coordinate system;
          * - :py:attr:`~cid`
            - Get or set the Local coordinate system ID number. If the number is zero, the global coordinate system is used.
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

    from constrained_linear_local import ConstrainedLinearLocal

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the ID for linear constraint definition
















   ..
       !! processed by numpydoc !!

.. py:property:: nid
   :type: Optional[int]


   
   Get or set the Node ID
















   ..
       !! processed by numpydoc !!

.. py:property:: dof
   :type: Optional[int]


   
   Get or set the Degrees of freedom in the local coordinate system;
   EQ.1: displacement along local x-direction
   EQ.2: displacement along local y-direction
   EQ.3: displacement along local z-direction
   EQ.4: local rotation about local x-axis
   EQ.5: local rotation about local y-axis
   EQ.6: local rotation about local z-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Local coordinate system ID number. If the number is zero, the global coordinate system is used.
















   ..
       !! processed by numpydoc !!

.. py:property:: coef
   :type: Optional[float]


   
   Get or set the Nonzero coefficient, Ck
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'LINEAR_LOCAL'






