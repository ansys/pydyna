





:class:`IcfdBoundaryFluxTemp`
=============================


.. py:class:: icfd_boundary_flux_temp.IcfdBoundaryFluxTemp(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_FLUX_TEMP keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryFluxTemp

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID of the fluid surface in contact with the solid domain
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe the temperature flux value versus time, see *DEFINE_CURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor. (default=1.0)
          * - :py:attr:`~death`
            - Get or set the Time at which the imposed motion/constraint is removed: EQ.0.0: default set to 1e28
          * - :py:attr:`~birth`
            - Get or set the Time at which the imposed pressure is activated starting from the initial abscissa value of the curve


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

    from icfd_boundary_flux_temp import IcfdBoundaryFluxTemp

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID of the fluid surface in contact with the solid domain
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the temperature flux value versus time, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor. (default=1.0)
















   ..
       !! processed by numpydoc !!

.. py:property:: death
   :type: float


   
   Get or set the Time at which the imposed motion/constraint is removed: EQ.0.0: default set to 1e28
















   ..
       !! processed by numpydoc !!

.. py:property:: birth
   :type: float


   
   Get or set the Time at which the imposed pressure is activated starting from the initial abscissa value of the curve
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_FLUX_TEMP'






