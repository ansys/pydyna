





:class:`InterfaceCompensation3DRefineRigid`
===========================================


.. py:class:: interface_compensation_3d_refine_rigid.InterfaceCompensation3DRefineRigid(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_COMPENSATION_3D_REFINE_RIGID keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceCompensation3DRefineRigid

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename1`
            - Get or set the A keyword file name of rigid tool mesh to be refined.  This should be the tooling mesh used in the forming or flanging simulation, before any compensation is done.  The refined rigid tool mesh will be in the file rigid_refined.tmp.  See keyword example.
          * - :py:attr:`~filename2`
            - Get or set the A keyword file name with trim curves defined using *DEFINE_CURVE_TRIM_3D.  The curves will be used to refine and realign the FILENAME1 to improve the convergence in the iterative compensation process.  The refined rigid tool mesh will be in the file rigid_refined.tmp.  See keyword example.


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

    from interface_compensation_3d_refine_rigid import InterfaceCompensation3DRefineRigid

Property detail
---------------

.. py:property:: filename1
   :type: Optional[str]


   
   Get or set the A keyword file name of rigid tool mesh to be refined.  This should be the tooling mesh used in the forming or flanging simulation, before any compensation is done.  The refined rigid tool mesh will be in the file rigid_refined.tmp.  See keyword example.
















   ..
       !! processed by numpydoc !!

.. py:property:: filename2
   :type: Optional[str]


   
   Get or set the A keyword file name with trim curves defined using *DEFINE_CURVE_TRIM_3D.  The curves will be used to refine and realign the FILENAME1 to improve the convergence in the iterative compensation process.  The refined rigid tool mesh will be in the file rigid_refined.tmp.  See keyword example.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'COMPENSATION_3D_REFINE_RIGID'






