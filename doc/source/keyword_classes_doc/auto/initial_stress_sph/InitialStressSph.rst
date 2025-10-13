





:class:`InitialStressSph`
=========================


.. py:class:: initial_stress_sph.InitialStressSph(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INITIAL_STRESS_SPH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressSph

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the SPH particle ID.
          * - :py:attr:`~sigxx`
            - Get or set the Define the xx stress component (global cartesian system).
          * - :py:attr:`~sigyy`
            - Get or set the Define the yy stress component (global cartesian system).
          * - :py:attr:`~sigzz`
            - Get or set the Define the zz stress component (global cartesian system).
          * - :py:attr:`~sigxy`
            - Get or set the Define the xy stress component (global cartesian system).
          * - :py:attr:`~sigyz`
            - Get or set the Define the yz stress component (global cartesian system).
          * - :py:attr:`~sigzx`
            - Get or set the Define the zx stress component (global cartesian system).
          * - :py:attr:`~eps`
            - Get or set the Effective plastic strain.


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

    from initial_stress_sph import InitialStressSph

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the SPH particle ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxx
   :type: float


   
   Get or set the Define the xx stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyy
   :type: float


   
   Get or set the Define the yy stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigzz
   :type: float


   
   Get or set the Define the zz stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigxy
   :type: float


   
   Get or set the Define the xy stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigyz
   :type: float


   
   Get or set the Define the yz stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: sigzx
   :type: float


   
   Get or set the Define the zx stress component (global cartesian system).
















   ..
       !! processed by numpydoc !!

.. py:property:: eps
   :type: float


   
   Get or set the Effective plastic strain.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INITIAL'


.. py:attribute:: subkeyword
   :value: 'STRESS_SPH'






