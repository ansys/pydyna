





:class:`IcfdMat`
================


.. py:class:: icfd_mat.IcfdMat(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_MAT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdMat

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material ID.
          * - :py:attr:`~flg`
            - Get or set the Flag to choose between fully incompressible, slightly compressible,or barotropic flows.
          * - :py:attr:`~ro`
            - Get or set the Flow density.
          * - :py:attr:`~vis`
            - Get or set the Dynamic viscosity.
          * - :py:attr:`~st`
            - Get or set the Surface tension coefficient.
          * - :py:attr:`~stsflcid`
            - Get or set the Load curve ID for scale factor applied on ST function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
          * - :py:attr:`~ca`
            - Get or set the Contact angle
          * - :py:attr:`~hc`
            - Get or set the Heat capacity.
          * - :py:attr:`~tc`
            - Get or set the Thermal conductivity.
          * - :py:attr:`~beta`
            - Get or set the Thermal expansion coefficient used in the Boussinesq approximation for buoyancy.
          * - :py:attr:`~prt`
            - Get or set the Turbulent Prandlt number. Only used if K-Epsilon turbulence model selected.
          * - :py:attr:`~hcsflcid`
            - Get or set the Load curve ID for scale factor applied on HC function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION
          * - :py:attr:`~tcsflcid`
            - Get or set the Load curve ID for scale factor applied on TC function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION
          * - :py:attr:`~nnmoid`
            - Get or set the Non-Newtonian model ID. This refers to a Non-Newtonian fluid model defined using *ICFD_MODEL_NONNEWT.
          * - :py:attr:`~pmmoid`
            - Get or set the Porous media model ID. This refers to a porous media model defined using *ICFD_MODEL_POROUS.


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

    from icfd_mat import IcfdMat

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: flg
   :type: int


   
   Get or set the Flag to choose between fully incompressible, slightly compressible,or barotropic flows.
   EQ.0 : Vacuum (free surface problems only)
   EQ.1 : Fully incompressible fluid.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: float


   
   Get or set the Flow density.
















   ..
       !! processed by numpydoc !!

.. py:property:: vis
   :type: float


   
   Get or set the Dynamic viscosity.
















   ..
       !! processed by numpydoc !!

.. py:property:: st
   :type: float


   
   Get or set the Surface tension coefficient.
















   ..
       !! processed by numpydoc !!

.. py:property:: stsflcid
   :type: Optional[int]


   
   Get or set the Load curve ID for scale factor applied on ST function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed:  f(x,y,z,vx,vy,vz,temp,pres,time).
















   ..
       !! processed by numpydoc !!

.. py:property:: ca
   :type: float


   
   Get or set the Contact angle
















   ..
       !! processed by numpydoc !!

.. py:property:: hc
   :type: float


   
   Get or set the Heat capacity.
















   ..
       !! processed by numpydoc !!

.. py:property:: tc
   :type: float


   
   Get or set the Thermal conductivity.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: float


   
   Get or set the Thermal expansion coefficient used in the Boussinesq approximation for buoyancy.
















   ..
       !! processed by numpydoc !!

.. py:property:: prt
   :type: float


   
   Get or set the Turbulent Prandlt number. Only used if K-Epsilon turbulence model selected.
















   ..
       !! processed by numpydoc !!

.. py:property:: hcsflcid
   :type: Optional[int]


   
   Get or set the Load curve ID for scale factor applied on HC function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION
















   ..
       !! processed by numpydoc !!

.. py:property:: tcsflcid
   :type: Optional[int]


   
   Get or set the Load curve ID for scale factor applied on TC function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION
















   ..
       !! processed by numpydoc !!

.. py:property:: nnmoid
   :type: Optional[int]


   
   Get or set the Non-Newtonian model ID. This refers to a Non-Newtonian fluid model defined using *ICFD_MODEL_NONNEWT.
















   ..
       !! processed by numpydoc !!

.. py:property:: pmmoid
   :type: Optional[int]


   
   Get or set the Porous media model ID. This refers to a porous media model defined using *ICFD_MODEL_POROUS.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'MAT'






