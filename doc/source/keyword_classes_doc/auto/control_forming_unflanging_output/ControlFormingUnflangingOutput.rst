





:class:`ControlFormingUnflangingOutput`
=======================================


.. py:class:: control_forming_unflanging_output.ControlFormingUnflangingOutput(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONTROL_FORMING_UNFLANGING_OUTPUT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ControlFormingUnflangingOutput

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~thmx`
            - Get or set the Maximum thickness beyond which elements are deleted; this is
          * - :py:attr:`~thmn`
            - Get or set the Minimum thickness below which elements are deleted; this is useful
          * - :py:attr:`~epsmx`
            - Get or set the Maximum effective plastic strain beyond which elements are deleted,


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

    from control_forming_unflanging_output import ControlFormingUnflangingOutput

Property detail
---------------

.. py:property:: thmx
   :type: float


   
   Get or set the Maximum thickness beyond which elements are deleted; this is
   useful in removing wrinkling areas of the flange (shrink flange).
















   ..
       !! processed by numpydoc !!

.. py:property:: thmn
   :type: float


   
   Get or set the Minimum thickness below which elements are deleted; this is useful
   in removing overly thinned areas of the flange (stretch flange).
















   ..
       !! processed by numpydoc !!

.. py:property:: epsmx
   :type: float


   
   Get or set the Maximum effective plastic strain beyond which elements are deleted,
   this is useful in removing flange areas with high effective plastic strains (stretch flange).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONTROL'


.. py:attribute:: subkeyword
   :value: 'FORMING_UNFLANGING_OUTPUT'






