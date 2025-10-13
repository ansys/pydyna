





:class:`DefineHazProperties`
============================


.. py:class:: define_haz_properties.DefineHazProperties(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_HAZ_PROPERTIES keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineHazProperties

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id_haz`
            - Get or set the Property set ID. A unique ID number must be used.
          * - :py:attr:`~iop`
            - Get or set the Activity flag. If IOP = 0, then the scaling is not applied, and if IOP = 1, the scaling is active.
          * - :py:attr:`~pid`
            - Get or set the Part or part set ID.
          * - :py:attr:`~pid_typ`
            - Get or set the PID type. PID_TYP = 0 indicates that PID is a *PART ID, and PID_TYP = 1, a part set..
          * - :py:attr:`~iss`
            - Get or set the Curve ID for scaling the yield stress based on the distance to the closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
          * - :py:attr:`~ifs`
            - Get or set the Curve ID for scaling the failure strain based on the distance to the     closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
          * - :py:attr:`~isb`
            - Get or set the Curve ID for scaling the yield stress based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
          * - :py:attr:`~ifb`
            - Get or set the Curve ID for scaling the failure strain based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
          * - :py:attr:`~isc`
            - Get or set the Curve ID for scaling the yield stress based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
          * - :py:attr:`~ifc`
            - Get or set the Curve ID for scaling the failure strain based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
          * - :py:attr:`~isw`
            - Get or set the Curve ID for scaling the yield stress based on the distance to the closest tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
          * - :py:attr:`~ifw`
            - Get or set the Curve ID for scaling the failure strain based on the distance to the tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
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

    from define_haz_properties import DefineHazProperties

Property detail
---------------

.. py:property:: id_haz
   :type: int


   
   Get or set the Property set ID. A unique ID number must be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: iop
   :type: int


   
   Get or set the Activity flag. If IOP = 0, then the scaling is not applied, and if IOP = 1, the scaling is active.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: int


   
   Get or set the Part or part set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid_typ
   :type: int


   
   Get or set the PID type. PID_TYP = 0 indicates that PID is a *PART ID, and PID_TYP = 1, a part set..
















   ..
       !! processed by numpydoc !!

.. py:property:: iss
   :type: int


   
   Get or set the Curve ID for scaling the yield stress based on the distance to the closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifs
   :type: int


   
   Get or set the Curve ID for scaling the failure strain based on the distance to the     closest solid element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: isb
   :type: int


   
   Get or set the Curve ID for scaling the yield stress based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifb
   :type: int


   
   Get or set the Curve ID for scaling the failure strain based on the distance to the closest beam element spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: isc
   :type: int


   
   Get or set the Curve ID for scaling the yield stress based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifc
   :type: int


   
   Get or set the Curve ID for scaling the failure strain based on the distance to the closest constrained spot weld.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: isw
   :type: int


   
   Get or set the Curve ID for scaling the yield stress based on the distance to the closest tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















   ..
       !! processed by numpydoc !!

.. py:property:: ifw
   :type: int


   
   Get or set the Curve ID for scaling the failure strain based on the distance to the tailor welded blank node.Use a negative ID for curves normalized by the spot weld diameter as described in the Remarks below.
















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
   :value: 'DEFINE'


.. py:attribute:: subkeyword
   :value: 'HAZ_PROPERTIES'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





