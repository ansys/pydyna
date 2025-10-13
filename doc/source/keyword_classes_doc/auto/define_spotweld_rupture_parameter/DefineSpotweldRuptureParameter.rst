





:class:`DefineSpotweldRuptureParameter`
=======================================


.. py:class:: define_spotweld_rupture_parameter.DefineSpotweldRuptureParameter(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_SPOTWELD_RUPTURE_PARAMETER keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineSpotweldRuptureParameter

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the Part ID for the attached shell..
          * - :py:attr:`~c11`
            - Get or set the Parameters for model.
          * - :py:attr:`~c12`
            - Get or set the Parameters for model.
          * - :py:attr:`~c13`
            - Get or set the Parameters for model.
          * - :py:attr:`~n11`
            - Get or set the Parameters for model.
          * - :py:attr:`~n12`
            - Get or set the Parameters for model.
          * - :py:attr:`~n13`
            - Get or set the Parameters for model.
          * - :py:attr:`~sig_pf`
            - Get or set the Nugget pull-out stress.
          * - :py:attr:`~c21`
            - Get or set the Parameters for model.
          * - :py:attr:`~c22`
            - Get or set the Parameters for model.
          * - :py:attr:`~c23`
            - Get or set the Parameters for model.
          * - :py:attr:`~n2`
            - Get or set the Parameters for model.
          * - :py:attr:`~sig_nf`
            - Get or set the Nugget fracture stress.
          * - :py:attr:`~lcdpa`
            - Get or set the Curve ID defining dynamic scale factor of spot weld axial load rate for nugget pull-out mode.
          * - :py:attr:`~lcdpm`
            - Get or set the Curve ID defining dynamic scale factor of spot weld moment
          * - :py:attr:`~lcdps`
            - Get or set the Curve ID defining dynamic scale factor of spot weld shear load
          * - :py:attr:`~lcdna`
            - Get or set the Curve ID defining dynamic scale factor of spot weld axial load
          * - :py:attr:`~lcdnm`
            - Get or set the Curve ID defining dynamic scale factor of spot weld moment
          * - :py:attr:`~lcdns`
            - Get or set the Curve ID defining dynamic scale factor of spot weld shear load
          * - :py:attr:`~nsmt`
            - Get or set the The number of time steps used for averaging the resultant rates
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

    from define_spotweld_rupture_parameter import DefineSpotweldRuptureParameter

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the Part ID for the attached shell..
















   ..
       !! processed by numpydoc !!

.. py:property:: c11
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: c12
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: c13
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: n11
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: n12
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: n13
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig_pf
   :type: Optional[float]


   
   Get or set the Nugget pull-out stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: c21
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: c22
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: c23
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2
   :type: Optional[float]


   
   Get or set the Parameters for model.
















   ..
       !! processed by numpydoc !!

.. py:property:: sig_nf
   :type: Optional[float]


   
   Get or set the Nugget fracture stress.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdpa
   :type: int


   
   Get or set the Curve ID defining dynamic scale factor of spot weld axial load rate for nugget pull-out mode.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdpm
   :type: int


   
   Get or set the Curve ID defining dynamic scale factor of spot weld moment
   load rate for nugget pull-out mode.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdps
   :type: int


   
   Get or set the Curve ID defining dynamic scale factor of spot weld shear load
   rate for nugget pull-out mode.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdna
   :type: int


   
   Get or set the Curve ID defining dynamic scale factor of spot weld axial load
   rate for nugget fracture mode.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdnm
   :type: int


   
   Get or set the Curve ID defining dynamic scale factor of spot weld moment
   load rate for nugget fracture mode.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcdns
   :type: int


   
   Get or set the Curve ID defining dynamic scale factor of spot weld shear load
   rate for nugget fracture mode
















   ..
       !! processed by numpydoc !!

.. py:property:: nsmt
   :type: int


   
   Get or set the The number of time steps used for averaging the resultant rates
   for the dynamic scale factors.
















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
   :value: 'SPOTWELD_RUPTURE_PARAMETER'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





