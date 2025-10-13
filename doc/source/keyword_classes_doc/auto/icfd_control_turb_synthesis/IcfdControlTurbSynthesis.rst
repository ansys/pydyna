





:class:`IcfdControlTurbSynthesis`
=================================


.. py:class:: icfd_control_turb_synthesis.IcfdControlTurbSynthesis(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_CONTROL_TURB_SYNTHESIS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdControlTurbSynthesis

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID of the surface with the turbulent velocity inlet condition.
          * - :py:attr:`~iu`
            - Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
          * - :py:attr:`~iv`
            - Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
          * - :py:attr:`~iw`
            - Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
          * - :py:attr:`~ls`
            - Get or set the Integral length scale of turbulence


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

    from icfd_control_turb_synthesis import IcfdControlTurbSynthesis

Property detail
---------------

.. py:property:: pid
   :type: int


   
   Get or set the Part ID of the surface with the turbulent velocity inlet condition.
















   ..
       !! processed by numpydoc !!

.. py:property:: iu
   :type: float


   
   Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
















   ..
       !! processed by numpydoc !!

.. py:property:: iv
   :type: float


   
   Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
















   ..
       !! processed by numpydoc !!

.. py:property:: iw
   :type: float


   
   Get or set the Intensity of field fluctuations (in %) over x,y,z directions.
















   ..
       !! processed by numpydoc !!

.. py:property:: ls
   :type: Optional[float]


   
   Get or set the Integral length scale of turbulence
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'CONTROL_TURB_SYNTHESIS'






