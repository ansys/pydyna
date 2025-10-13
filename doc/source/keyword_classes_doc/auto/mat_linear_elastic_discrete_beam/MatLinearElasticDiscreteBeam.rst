





:class:`MatLinearElasticDiscreteBeam`
=====================================


.. py:class:: mat_linear_elastic_discrete_beam.MatLinearElasticDiscreteBeam(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_LINEAR_ELASTIC_DISCRETE_BEAM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatLinearElasticDiscreteBeam

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. A unique number has to be used.
          * - :py:attr:`~ro`
            - Get or set the Mass density, see also volume in the *SECTION_BEAM definition.
          * - :py:attr:`~tkr`
            - Get or set the Translational stiffness about local r-axis.
          * - :py:attr:`~tks`
            - Get or set the Translational stiffness about local s-axis.
          * - :py:attr:`~tkt`
            - Get or set the Translational stiffness about local t-axis.
          * - :py:attr:`~rkr`
            - Get or set the Rotational stiffness about the local r-axis.
          * - :py:attr:`~rks`
            - Get or set the Rotational stiffness about the local s-axis.
          * - :py:attr:`~rkt`
            - Get or set the Rotational stiffness about the local t-axis.
          * - :py:attr:`~tdr`
            - Get or set the Translational viscous damper about local r-axis (optional).
          * - :py:attr:`~tds`
            - Get or set the Translational viscous damper about local s-axis (optional).
          * - :py:attr:`~tdt`
            - Get or set the Translational viscous damper about local t-axis (opitonal).
          * - :py:attr:`~rdr`
            - Get or set the Rotational viscous damper about the local r-axis (optional).
          * - :py:attr:`~rds`
            - Get or set the Rotational viscous damper about the local s-axis (optional).
          * - :py:attr:`~rdt`
            - Get or set the Rotational viscous damper about the local t-axis (optional).
          * - :py:attr:`~for_`
            - Get or set the Preload force in r-direction
          * - :py:attr:`~fos`
            - Get or set the Preload force in s-direction
          * - :py:attr:`~fot`
            - Get or set the Preload force in t-direction
          * - :py:attr:`~mor`
            - Get or set the Preload moment about r-axis
          * - :py:attr:`~mos`
            - Get or set the Preload moment about s-axis
          * - :py:attr:`~mot`
            - Get or set the Preload moment about t-axis
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from mat_linear_elastic_discrete_beam import MatLinearElasticDiscreteBeam

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density, see also volume in the *SECTION_BEAM definition.
















   ..
       !! processed by numpydoc !!

.. py:property:: tkr
   :type: Optional[float]


   
   Get or set the Translational stiffness about local r-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: tks
   :type: Optional[float]


   
   Get or set the Translational stiffness about local s-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: tkt
   :type: Optional[float]


   
   Get or set the Translational stiffness about local t-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: rkr
   :type: Optional[float]


   
   Get or set the Rotational stiffness about the local r-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: rks
   :type: Optional[float]


   
   Get or set the Rotational stiffness about the local s-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: rkt
   :type: Optional[float]


   
   Get or set the Rotational stiffness about the local t-axis.
















   ..
       !! processed by numpydoc !!

.. py:property:: tdr
   :type: Optional[float]


   
   Get or set the Translational viscous damper about local r-axis (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: tds
   :type: Optional[float]


   
   Get or set the Translational viscous damper about local s-axis (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: tdt
   :type: Optional[float]


   
   Get or set the Translational viscous damper about local t-axis (opitonal).
















   ..
       !! processed by numpydoc !!

.. py:property:: rdr
   :type: Optional[float]


   
   Get or set the Rotational viscous damper about the local r-axis (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: rds
   :type: Optional[float]


   
   Get or set the Rotational viscous damper about the local s-axis (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: rdt
   :type: Optional[float]


   
   Get or set the Rotational viscous damper about the local t-axis (optional).
















   ..
       !! processed by numpydoc !!

.. py:property:: for_
   :type: Optional[float]


   
   Get or set the Preload force in r-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: fos
   :type: Optional[float]


   
   Get or set the Preload force in s-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: fot
   :type: Optional[float]


   
   Get or set the Preload force in t-direction
















   ..
       !! processed by numpydoc !!

.. py:property:: mor
   :type: Optional[float]


   
   Get or set the Preload moment about r-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: mos
   :type: Optional[float]


   
   Get or set the Preload moment about s-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: mot
   :type: Optional[float]


   
   Get or set the Preload moment about t-axis
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'MAT'


.. py:attribute:: subkeyword
   :value: 'LINEAR_ELASTIC_DISCRETE_BEAM'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





