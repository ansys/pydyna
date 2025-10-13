





:class:`IcfdBoundaryPrescribedLevelset`
=======================================


.. py:class:: icfd_boundary_prescribed_levelset.IcfdBoundaryPrescribedLevelset(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ICFD_BOUNDARY_PRESCRIBED_LEVELSET keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IcfdBoundaryPrescribedLevelset

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~pid`
            - Get or set the PID of the fluid surface where a fluid height will be imposed.
          * - :py:attr:`~ptid`
            - Get or set the Point ID specifying the origin of the fluid surface. See ICFD_DEFINE_POINT.
          * - :py:attr:`~axe`
            - Get or set the Global axis specifying the direction of the fluid (X=1, Y=2, Z=3).


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

    from icfd_boundary_prescribed_levelset import IcfdBoundaryPrescribedLevelset

Property detail
---------------

.. py:property:: pid
   :type: Optional[int]


   
   Get or set the PID of the fluid surface where a fluid height will be imposed.
















   ..
       !! processed by numpydoc !!

.. py:property:: ptid
   :type: Optional[int]


   
   Get or set the Point ID specifying the origin of the fluid surface. See ICFD_DEFINE_POINT.
















   ..
       !! processed by numpydoc !!

.. py:property:: axe
   :type: Optional[int]


   
   Get or set the Global axis specifying the direction of the fluid (X=1, Y=2, Z=3).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ICFD'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_PRESCRIBED_LEVELSET'






