





:class:`IntegrationShell`
=========================


.. py:class:: integration_shell.IntegrationShell(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTEGRATION_SHELL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IntegrationShell

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~irid`
            - Get or set the Integration rule ID (IRID refers to IRID on *SECTION_SHELL card).
          * - :py:attr:`~nip`
            - Get or set the Number of integration points.
          * - :py:attr:`~esop`
            - Get or set the Equal spacing of integration points option:
          * - :py:attr:`~failopt`
            - Get or set the Treatment of failure when mixing different constitutive types, which do and do not include failure models, through the shell thickness.
          * - :py:attr:`~s`
            - Get or set the Coordinate of integration point in range -1.0 to 1.0.
          * - :py:attr:`~wf`
            - Get or set the Weighting factor. This is typically the thickness associated with the integration point divided by actual shell thickness, i.e., the weighting factor for the ith integration point = dti/t.
          * - :py:attr:`~pid`
            - Get or set the Optional part ID if different from the PID specified on the element card.  The material type is not allowed to change, see *PART. The average mass density for the shell element is based on a weighted average of the density of each layer that is used through the thickness. When modifying the constitutive constants throuigh the thickness, it is often necessary to defined unique part IDs without elements that are referenced only by the user integration rule. These additional part IDs only provide a density and constitutive constants with local material axes (if used) and orientation angles taken from the PID referenced on the element card. In defining a PID for an integration point, it is okay to reference a solid element PID.


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

    from integration_shell import IntegrationShell

Property detail
---------------

.. py:property:: irid
   :type: Optional[int]


   
   Get or set the Integration rule ID (IRID refers to IRID on *SECTION_SHELL card).
















   ..
       !! processed by numpydoc !!

.. py:property:: nip
   :type: Optional[int]


   
   Get or set the Number of integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: esop
   :type: int


   
   Get or set the Equal spacing of integration points option:
   EQ.0: integration points are defined below (default),
   EQ.1: integration points are equally spaced through thickness such that the shell is subdivided into NIP layers of equal thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: failopt
   :type: int


   
   Get or set the Treatment of failure when mixing different constitutive types, which do and do not include failure models, through the shell thickness.
   EQ.0: element is deleted when layers which include failure, fail
   EQ.1: element failure cannot occur since some layers do not have a failure option
















   ..
       !! processed by numpydoc !!

.. py:property:: s
   :type: Optional[float]


   
   Get or set the Coordinate of integration point in range -1.0 to 1.0.
















   ..
       !! processed by numpydoc !!

.. py:property:: wf
   :type: Optional[float]


   
   Get or set the Weighting factor. This is typically the thickness associated with the integration point divided by actual shell thickness, i.e., the weighting factor for the ith integration point = dti/t.
















   ..
       !! processed by numpydoc !!

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Optional part ID if different from the PID specified on the element card.  The material type is not allowed to change, see *PART. The average mass density for the shell element is based on a weighted average of the density of each layer that is used through the thickness. When modifying the constitutive constants throuigh the thickness, it is often necessary to defined unique part IDs without elements that are referenced only by the user integration rule. These additional part IDs only provide a density and constitutive constants with local material axes (if used) and orientation angles taken from the PID referenced on the element card. In defining a PID for an integration point, it is okay to reference a solid element PID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTEGRATION'


.. py:attribute:: subkeyword
   :value: 'SHELL'






