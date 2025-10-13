





:class:`MatViscoelastic`
========================


.. py:class:: mat_viscoelastic.MatViscoelastic(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA MAT_VISCOELASTIC keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: MatViscoelastic

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
            - Get or set the Mass density.
          * - :py:attr:`~bulk`
            - Get or set the Elastic bulk modulus.
          * - :py:attr:`~g0`
            - Get or set the Short-time shear modulus.
          * - :py:attr:`~gi`
            - Get or set the Long-time (infinite) shear modulus.
          * - :py:attr:`~beta`
            - Get or set the Decay constant.
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

    from mat_viscoelastic import MatViscoelastic

Property detail
---------------

.. py:property:: mid
   :type: Optional[int]


   
   Get or set the Material identification. A unique number has to be used.
















   ..
       !! processed by numpydoc !!

.. py:property:: ro
   :type: Optional[float]


   
   Get or set the Mass density.
















   ..
       !! processed by numpydoc !!

.. py:property:: bulk
   :type: Optional[float]


   
   Get or set the Elastic bulk modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: g0
   :type: Optional[float]


   
   Get or set the Short-time shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: gi
   :type: Optional[float]


   
   Get or set the Long-time (infinite) shear modulus.
















   ..
       !! processed by numpydoc !!

.. py:property:: beta
   :type: Optional[float]


   
   Get or set the Decay constant.
















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
   :value: 'VISCOELASTIC'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





