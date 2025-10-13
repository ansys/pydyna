





:class:`ElementLancing`
=======================


.. py:class:: element_lancing.ElementLancing(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ELEMENT_LANCING keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ElementLancing

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idpt`
            - Get or set the PID of the sheet blank to be lanced, see *PART.
          * - :py:attr:`~idcv`
            - Get or set the Curve ID (the variable TCID in *DEFINE_CURVE_TRIM_3D) defining a lancing route (see Remarks)..
          * - :py:attr:`~irefine`
            - Get or set the Set IREFINE = 1 to refine elements along lancing route until no
          * - :py:attr:`~smin`
            - Get or set the Minimum element characteristic length to be refined to, to be
          * - :py:attr:`~at`
            - Get or set the Activation time for lancing operation. This variable needs to be
          * - :py:attr:`~endt`
            - Get or set the End time (for progressive lancing only).
          * - :py:attr:`~ntimes`
            - Get or set the A progressive lancing operation is evenly divided into NTIMES


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

    from element_lancing import ElementLancing

Property detail
---------------

.. py:property:: idpt
   :type: Optional[int]


   
   Get or set the PID of the sheet blank to be lanced, see *PART.
















   ..
       !! processed by numpydoc !!

.. py:property:: idcv
   :type: Optional[int]


   
   Get or set the Curve ID (the variable TCID in *DEFINE_CURVE_TRIM_3D) defining a lancing route (see Remarks)..
















   ..
       !! processed by numpydoc !!

.. py:property:: irefine
   :type: Optional[int]


   
   Get or set the Set IREFINE = 1 to refine elements along lancing route until no
   adapted nodes exist in the neighborhood. This feature result in a
   more robust lancing in the form of improved lancing boundary.
   Available starting in Revision 107708..
















   ..
       !! processed by numpydoc !!

.. py:property:: smin
   :type: Optional[float]


   
   Get or set the Minimum element characteristic length to be refined to, to be
   supported in the future. Currently, no refinement will be made..
















   ..
       !! processed by numpydoc !!

.. py:property:: at
   :type: Optional[float]


   
   Get or set the Activation time for lancing operation. This variable needs to be
   defined for both instant and progressive lancing types (see Remarks).
















   ..
       !! processed by numpydoc !!

.. py:property:: endt
   :type: Optional[float]


   
   Get or set the End time (for progressive lancing only).
















   ..
       !! processed by numpydoc !!

.. py:property:: ntimes
   :type: Optional[int]


   
   Get or set the A progressive lancing operation is evenly divided into NTIMES
   segments between AT and ENDT; within each segment lancing is
   done instantly. Do not define for instant lancing.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ELEMENT'


.. py:attribute:: subkeyword
   :value: 'LANCING'






