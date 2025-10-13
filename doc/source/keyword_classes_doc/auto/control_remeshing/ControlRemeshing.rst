





:class:`ControlRemeshing`
=========================


.. py:class:: control_remeshing.ControlRemeshing(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_REMESHING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlRemeshing

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~rmin`
            - Get or set the Minimum edge length for the surface mesh surrounding the parts which should be remeshed.
          * - :py:attr:`~rmax`
            - Get or set the Maximum edge length for the surface mesh surrounding the parts which should be remeshed.
          * - :py:attr:`~vf_loss`
            - Get or set the Volume fraction loss required in a type 10/13 tetrahedral elements to trigger a remesh.  For the type 13 solid elements, the pressures are computed at the nodal points; therefore, it is possible for overall volume to be conserved but for individual tetrahedrons to experience a significant volume loss or gain.  The volume loss can lead to numerical problems.  Recommended values for VF_‌LOSS in the range of 0.10 to 0.30 may be reasonable.
          * - :py:attr:`~mfrac`
            - Get or set the Mass ratio gain during mass scaling required for triggering a remesh. For a one percent increase in mass, set MFRAC=0.01.
          * - :py:attr:`~dt_min`
            - Get or set the Time-step size required for triggering a remesh. This option is checked before mass scaling is applied and the time step size reset.
          * - :py:attr:`~icurv`
            - Get or set the Define number of element along the radius in the adaptivity.
          * - :py:attr:`~iadp10`
            - Get or set the Not used.
          * - :py:attr:`~sefang`
            - Get or set the Define angular mesh size in 3-D axisymmetric remeshing.


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

    from control_remeshing import ControlRemeshing

Property detail
---------------

.. py:property:: rmin
   :type: Optional[float]


   
   Get or set the Minimum edge length for the surface mesh surrounding the parts which should be remeshed.
















   ..
       !! processed by numpydoc !!

.. py:property:: rmax
   :type: Optional[float]


   
   Get or set the Maximum edge length for the surface mesh surrounding the parts which should be remeshed.
















   ..
       !! processed by numpydoc !!

.. py:property:: vf_loss
   :type: float


   
   Get or set the Volume fraction loss required in a type 10/13 tetrahedral elements to trigger a remesh.  For the type 13 solid elements, the pressures are computed at the nodal points; therefore, it is possible for overall volume to be conserved but for individual tetrahedrons to experience a significant volume loss or gain.  The volume loss can lead to numerical problems.  Recommended values for VF_‌LOSS in the range of 0.10 to 0.30 may be reasonable.
















   ..
       !! processed by numpydoc !!

.. py:property:: mfrac
   :type: float


   
   Get or set the Mass ratio gain during mass scaling required for triggering a remesh. For a one percent increase in mass, set MFRAC=0.01.
















   ..
       !! processed by numpydoc !!

.. py:property:: dt_min
   :type: float


   
   Get or set the Time-step size required for triggering a remesh. This option is checked before mass scaling is applied and the time step size reset.
















   ..
       !! processed by numpydoc !!

.. py:property:: icurv
   :type: int


   
   Get or set the Define number of element along the radius in the adaptivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: iadp10
   :type: int


   
   Get or set the Not used.
















   ..
       !! processed by numpydoc !!

.. py:property:: sefang
   :type: float


   
   Get or set the Define angular mesh size in 3-D axisymmetric remeshing.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'REMESHING'






