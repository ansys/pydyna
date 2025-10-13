





:class:`IncludeWdWeldlineCurve`
===============================


.. py:class:: include_wd_weldline_curve.IncludeWdWeldlineCurve(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INCLUDE_WD_WELDLINE_CURVE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: IncludeWdWeldlineCurve

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~filename`
            - Get or set the File name of the welding curve in keyword format (see *DEFINE_CURVE_TRIM_3D). If IOPTION = 1 on *INTERFACE_WELDLINE_DEVELOPMENT, it should define the final welding curve. If IOPTION = -1, it should define the initial welding curve


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

    from include_wd_weldline_curve import IncludeWdWeldlineCurve

Property detail
---------------

.. py:property:: filename
   :type: Optional[str]


   
   Get or set the File name of the welding curve in keyword format (see *DEFINE_CURVE_TRIM_3D). If IOPTION = 1 on *INTERFACE_WELDLINE_DEVELOPMENT, it should define the final welding curve. If IOPTION = -1, it should define the initial welding curve
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INCLUDE'


.. py:attribute:: subkeyword
   :value: 'WD_WELDLINE_CURVE'






