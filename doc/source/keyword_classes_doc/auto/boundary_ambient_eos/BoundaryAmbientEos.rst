





:class:`BoundaryAmbientEos`
===========================


.. py:class:: boundary_ambient_eos.BoundaryAmbientEos(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA BOUNDARY_AMBIENT_EOS keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: BoundaryAmbientEos

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the The ambient Part ID for which the thermodynamic state is being defined.
          * - :py:attr:`~lc1`
            - Get or set the Load curve ID for internal energy per unit reference specific volume (or a temperature load curve ID if *EOS_IDEAL_GAS is being used).
          * - :py:attr:`~lc2`
            - Get or set the Load curve ID for relative volume, see *DEFINE_CURVE.


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

    from boundary_ambient_eos import BoundaryAmbientEos

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the The ambient Part ID for which the thermodynamic state is being defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: lc1
   :type: Optional[int]


   
   Get or set the Load curve ID for internal energy per unit reference specific volume (or a temperature load curve ID if *EOS_IDEAL_GAS is being used).
















   ..
       !! processed by numpydoc !!

.. py:property:: lc2
   :type: Optional[int]


   
   Get or set the Load curve ID for relative volume, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'BOUNDARY'


.. py:attribute:: subkeyword
   :value: 'AMBIENT_EOS'






