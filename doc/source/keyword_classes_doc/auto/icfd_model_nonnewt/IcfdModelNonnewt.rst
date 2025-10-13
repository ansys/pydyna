





:class:`IcfdModelNonnewt`
=========================


.. py:class:: icfd_model_nonnewt.IcfdModelNonnewt(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_MODEL_NONNEWT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdModelNonnewt

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~nnmoid`
            - Get or set the Non-Newtonian Model ID.
          * - :py:attr:`~nnid`
            - Get or set the Non-Newtonian fluid model type:
          * - :py:attr:`~k`
            - Get or set the Consistency index if NNID = 1 and 4. Zero shear Viscosity if NNID = 2,3 and 5.Reference viscosity if NNID = 6 and NNID = 7. Load curve ID or function ID if NNID = 8.
          * - :py:attr:`~n`
            - Get or set the Measure of the deviation of the fluid from Newtonian (Power Law index) for NNID = 1,2,3,4,5,7. Not used for NNID = 6 and 8.
          * - :py:attr:`~mumin`
            - Get or set the Minimum acceptable viscosity value if NNID = 1. Infinite Shear Viscosity if NNID = 2,5.Yielding viscosity if NNID = 4.Not used if NNID = 3,6,7,8.
          * - :py:attr:`~lambda_`
            - Get or set the Maximum acceptable viscosity value if NNID = 1. Time constant if NNID = 2, 3, 5. Yield Stress Threshold if NNID = 4.Sutherland constant if NNID = 6. Not used if NNID = 7,8.
          * - :py:attr:`~alpha`
            - Get or set the Activation energy if NNID = 1, 2. Not used if NNID = 3,4,5,6,7,8
          * - :py:attr:`~talpha`
            - Get or set the Reference temperature if NNID = 2. Not used if NNID = 1,3,4,5,6,7,8


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

    from icfd_model_nonnewt import IcfdModelNonnewt

Property detail
---------------

.. py:property:: nnmoid
   :type: Optional[int]


   
   Get or set the Non-Newtonian Model ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nnid
   :type: int


   
   Get or set the Non-Newtonian fluid model type:
   EQ.1 : Power-Law model.
   EQ.2 : Carreau model.
   EQ.3 : Cross model.
   EQ.4 : Herschel-Bulkley model.
   EQ.5 : Cross II model.
   EQ.6 : Sutherland formula for temperature dependent viscosity.
   EQ.7 : Power-Law for temperature dependent viscosity.
   EQ.8 : Viscosity defined by Load Curve ID or Function ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: k
   :type: float


   
   Get or set the Consistency index if NNID = 1 and 4. Zero shear Viscosity if NNID = 2,3 and 5.Reference viscosity if NNID = 6 and NNID = 7. Load curve ID or function ID if NNID = 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: n
   :type: float


   
   Get or set the Measure of the deviation of the fluid from Newtonian (Power Law index) for NNID = 1,2,3,4,5,7. Not used for NNID = 6 and 8.
















   ..
       !! processed by numpydoc !!

.. py:property:: mumin
   :type: float


   
   Get or set the Minimum acceptable viscosity value if NNID = 1. Infinite Shear Viscosity if NNID = 2,5.Yielding viscosity if NNID = 4.Not used if NNID = 3,6,7,8.
















   ..
       !! processed by numpydoc !!

.. py:property:: lambda_
   :type: float


   
   Get or set the Maximum acceptable viscosity value if NNID = 1. Time constant if NNID = 2, 3, 5. Yield Stress Threshold if NNID = 4.Sutherland constant if NNID = 6. Not used if NNID = 7,8.
















   ..
       !! processed by numpydoc !!

.. py:property:: alpha
   :type: float


   
   Get or set the Activation energy if NNID = 1, 2. Not used if NNID = 3,4,5,6,7,8
















   ..
       !! processed by numpydoc !!

.. py:property:: talpha
   :type: float


   
   Get or set the Reference temperature if NNID = 2. Not used if NNID = 1,3,4,5,6,7,8
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'MODEL_NONNEWT'






