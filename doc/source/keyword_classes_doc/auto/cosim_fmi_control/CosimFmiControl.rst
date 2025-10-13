





:class:`CosimFmiControl`
========================


.. py:class:: cosim_fmi_control.CosimFmiControl(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA COSIM_FMI_CONTROL keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CosimFmiControl

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~appid`
            - Get or set the FMU identification. Each FMU must have a unique APPID
          * - :py:attr:`~opt`
            - Get or set the LS-DYNA's role (see Remark 1):
          * - :py:attr:`~mode`
            - Get or set the LS-DYNA's mode
          * - :py:attr:`~fmi`
            - Get or set the FMI1.0 or FMI2.0
          * - :py:attr:`~setting`
            - Get or set the Settings for the generating or using the FMU. Up to 80 characters per line. Multiple lines are allowed for each setting and multiple settings can be defined for each FMU


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

    from cosim_fmi_control import CosimFmiControl

Property detail
---------------

.. py:property:: appid
   :type: Optional[str]


   
   Get or set the FMU identification. Each FMU must have a unique APPID
















   ..
       !! processed by numpydoc !!

.. py:property:: opt
   :type: str


   
   Get or set the LS-DYNA's role (see Remark 1):
   EQ.G:   Generation mode.LS - DYNA will generate a new FMU.
   EQ.C : Co - simulation mode.LS - DYNA will co - simulate with an existing FMU.
















   ..
       !! processed by numpydoc !!

.. py:property:: mode
   :type: str


   
   Get or set the LS-DYNA's mode
   EQ.P:   LS - DYNA is Primary,and another software is secondary.
   EQ.S : LS - DYNA is Secondary,and another software is primary..
















   ..
       !! processed by numpydoc !!

.. py:property:: fmi
   :type: int


   
   Get or set the FMI1.0 or FMI2.0
   EQ.1: the generated or co - simulated FMU is based on FMI1.0 standard.
   EQ.0 or 2 : the generated or co - simulated FMU is based on FMI2.0 standard.If the FMI option is left blank, the default FMU 2 will be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: setting
   :type: Optional[str]


   
   Get or set the Settings for the generating or using the FMU. Up to 80 characters per line. Multiple lines are allowed for each setting and multiple settings can be defined for each FMU
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'COSIM'


.. py:attribute:: subkeyword
   :value: 'FMI_CONTROL'






