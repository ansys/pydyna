





:class:`IcfdBoundaryPrescribedMovemesh`
=======================================


.. py:class:: icfd_boundary_prescribed_movemesh.IcfdBoundaryPrescribedMovemesh(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_PRESCRIBED_MOVEMESH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryPrescribedMovemesh

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID for a fluid surface.
          * - :py:attr:`~dofx`
            - Get or set the Degrees of freedom in the X,Y and Z directions :
          * - :py:attr:`~dofy`
            - Get or set the Degrees of freedom in the X,Y and Z directions :
          * - :py:attr:`~dofz`
            - Get or set the Degrees of freedom in the X,Y and Z directions :


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

    from icfd_boundary_prescribed_movemesh import IcfdBoundaryPrescribedMovemesh

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID for a fluid surface.
















   ..
       !! processed by numpydoc !!

.. py:property:: dofx
   :type: int


   
   Get or set the Degrees of freedom in the X,Y and Z directions :
   EQ.0: degree of freedom left free (Surface nodes can translate in the
   chosen direction)
   EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
















   ..
       !! processed by numpydoc !!

.. py:property:: dofy
   :type: int


   
   Get or set the Degrees of freedom in the X,Y and Z directions :
   EQ.0: degree of freedom left free (Surface nodes can translate in the
   chosen direction)
   EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
















   ..
       !! processed by numpydoc !!

.. py:property:: dofz
   :type: int


   
   Get or set the Degrees of freedom in the X,Y and Z directions :
   EQ.0: degree of freedom left free (Surface nodes can translate in the
   chosen direction)
   EQ. 1: prescribed degree of freedom (Surface nodes are blocked).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_MOVEMESH'






