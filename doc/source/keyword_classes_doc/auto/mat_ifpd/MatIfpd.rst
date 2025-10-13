





:class:`MatIfpd`
================


.. py:class:: mat_ifpd.MatIfpd(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_IFPD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatIfpd

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~mid`
            - Get or set the Material identification. MID is referenced on the *PART card. A unique number or label must be specified
          * - :py:attr:`~ro`
            - Get or set the Fluid density
          * - :py:attr:`~dynvis`
            - Get or set the Dynamic viscosity of the fluid
          * - :py:attr:`~sften`
            - Get or set the Surface tension coefficient of the fluid
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

    from mat_ifpd import MatIfpd

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. MID is referenced on the *PART card. A unique number or label must be specified
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Fluid density
















   ..
       !! processed by numpydoc !!

.. py:property:: dynvis
   :type: Optional[float]


   
   Get or set the Dynamic viscosity of the fluid
















   ..
       !! processed by numpydoc !!

.. py:property:: sften
   :type: Optional[float]


   
   Get or set the Surface tension coefficient of the fluid
















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
   :value: 'IFPD'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





