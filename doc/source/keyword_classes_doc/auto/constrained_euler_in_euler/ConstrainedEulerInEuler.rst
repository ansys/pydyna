





:class:`ConstrainedEulerInEuler`
================================


.. py:class:: constrained_euler_in_euler.ConstrainedEulerInEuler(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_EULER_IN_EULER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedEulerInEuler

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid1`
            - Get or set the Part set ID of the 1st ALE or Eulerian set of mesh(es.
          * - :py:attr:`~psid2`
            - Get or set the Part set ID of the 2nd ALE or Eulerian set of mesh(es).
          * - :py:attr:`~pfac`
            - Get or set the A penalty factor for the coupling interaction between the two PSIDs.


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

    from constrained_euler_in_euler import ConstrainedEulerInEuler

Property detail
---------------

.. py:property:: psid1
   :type: int


   
   Get or set the Part set ID of the 1st ALE or Eulerian set of mesh(es.
















   ..
       !! processed by numpydoc !!

.. py:property:: psid2
   :type: int


   
   Get or set the Part set ID of the 2nd ALE or Eulerian set of mesh(es).
















   ..
       !! processed by numpydoc !!

.. py:property:: pfac
   :type: float


   
   Get or set the A penalty factor for the coupling interaction between the two PSIDs.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'EULER_IN_EULER'






