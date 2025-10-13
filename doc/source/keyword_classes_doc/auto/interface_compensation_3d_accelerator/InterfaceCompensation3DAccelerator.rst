





:class:`InterfaceCompensation3DAccelerator`
===========================================


.. py:class:: interface_compensation_3d_accelerator.InterfaceCompensation3DAccelerator(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_COMPENSATION_3D_ACCELERATOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceCompensation3DAccelerator

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~isteps`
            - Get or set the Steps in accelerated compensation procedure, see Remarks.
          * - :py:attr:`~tolx`
            - Get or set the Part deviation tolerance between current blank and target blank shape in global x-direction.
          * - :py:attr:`~toly`
            - Get or set the Part deviation tolerance between current blank and target blank  shape in global y-direction.
          * - :py:attr:`~tolz`
            - Get or set the Part deviation tolerance between current blank and target blank  shape in global z-direction.
          * - :py:attr:`~option`
            - Get or set the Compensation acceleration method. Currently available only for method 1..


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

    from interface_compensation_3d_accelerator import InterfaceCompensation3DAccelerator

Property detail
---------------

.. py:property:: isteps
   :type: int


   
   Get or set the Steps in accelerated compensation procedure, see Remarks.
















   ..
       !! processed by numpydoc !!

.. py:property:: tolx
   :type: float


   
   Get or set the Part deviation tolerance between current blank and target blank shape in global x-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: toly
   :type: float


   
   Get or set the Part deviation tolerance between current blank and target blank  shape in global y-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: tolz
   :type: float


   
   Get or set the Part deviation tolerance between current blank and target blank  shape in global z-direction.
















   ..
       !! processed by numpydoc !!

.. py:property:: option
   :type: int


   
   Get or set the Compensation acceleration method. Currently available only for method 1..
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'COMPENSATION_3D_ACCELERATOR'






