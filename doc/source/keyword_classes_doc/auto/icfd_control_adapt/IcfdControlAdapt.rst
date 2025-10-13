





:class:`IcfdControlAdapt`
=========================


.. py:class:: icfd_control_adapt.IcfdControlAdapt(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_ADAPT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlAdapt

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~minh`
            - Get or set the Minimum mesh size allowed to the mesh generator. The resulting mesh will not have an element smaller than MINH even if the minimum size does not satisfy the maximum error.
          * - :py:attr:`~maxh`
            - Get or set the Maximum mesh size.
          * - :py:attr:`~err`
            - Get or set the Maximum perceptual error allowed in the whole domain.
          * - :py:attr:`~mth`
            - Get or set the Specify if the mesh size is computed based on function error or gradient error. EQ. 0: Function error. EQ. 1: Gradient error.
          * - :py:attr:`~nit`
            - Get or set the Number of iterations before a remeshing is forced:
          * - :py:attr:`~var`
            - Get or set the Specify which variable is taken into account for the error calculation:
          * - :py:attr:`~kis`
            - Get or set the Keep initial mesh size:


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

    from icfd_control_adapt import IcfdControlAdapt

Property detail
---------------

.. py:property:: minh
   :type: Optional[float]


   
   Get or set the Minimum mesh size allowed to the mesh generator. The resulting mesh will not have an element smaller than MINH even if the minimum size does not satisfy the maximum error.
















   ..
       !! processed by numpydoc !!

.. py:property:: maxh
   :type: Optional[float]


   
   Get or set the Maximum mesh size.
















   ..
       !! processed by numpydoc !!

.. py:property:: err
   :type: float


   
   Get or set the Maximum perceptual error allowed in the whole domain.
















   ..
       !! processed by numpydoc !!

.. py:property:: mth
   :type: int


   
   Get or set the Specify if the mesh size is computed based on function error or gradient error. EQ. 0: Function error. EQ. 1: Gradient error.
















   ..
       !! processed by numpydoc !!

.. py:property:: nit
   :type: int


   
   Get or set the Number of iterations before a remeshing is forced:
   GT.0:   Number of iterations before a forced remeshing
   EQ.0 : Do not remesh
   LT.0 : |NIT| is a load curve ID giving the number iterations before a remeshing as a function of time
















   ..
       !! processed by numpydoc !!

.. py:property:: var
   :type: int


   
   Get or set the Specify which variable is taken into account for the error calculation:
   EQ.0:   Velocity, pressure and levelset function are taken into account.
   EQ.1:   Remove the levelset function from the error calculation.
   EQ.2: Remove the pressure from the error calculation.
   EQ.3: Remove both pressure and levelset function. Only the fluid velocity will therefore remain.
















   ..
       !! processed by numpydoc !!

.. py:property:: kis
   :type: int


   
   Get or set the Keep initial mesh size:
   EQ.0:   Turned Off.The remeshing process will ignore the initial mesh size in the volume.
   EQ.1 : Turned on.Whenever a remeshing occurs, the new local mesh size will not be allowed to be substantially coarser than the one from the previous mesh.The object is to diminish the excessive coarsening that can occur between two remeshes
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_ADAPT'






