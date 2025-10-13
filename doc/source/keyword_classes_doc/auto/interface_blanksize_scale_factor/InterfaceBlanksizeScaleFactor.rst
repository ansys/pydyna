





:class:`InterfaceBlanksizeScaleFactor`
======================================


.. py:class:: interface_blanksize_scale_factor.InterfaceBlanksizeScaleFactor(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_BLANKSIZE_SCALE_FACTOR keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceBlanksizeScaleFactor

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~idcrv`
            - Get or set the Curve ID in the order of appearance as in FILENAME1 in the target card, as defined by *DEFINE_TARGET_BOUNDARY.
          * - :py:attr:`~sf`
            - Get or set the Scale factor for the IDCRV defined above.  It defines a fraction of the changes required for the predicted initial blank shape.
          * - :py:attr:`~offx`
            - Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
          * - :py:attr:`~offy`
            - Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
          * - :py:attr:`~offz`
            - Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.


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

    from interface_blanksize_scale_factor import InterfaceBlanksizeScaleFactor

Property detail
---------------

.. py:property:: idcrv
   :type: int


   
   Get or set the Curve ID in the order of appearance as in FILENAME1 in the target card, as defined by *DEFINE_TARGET_BOUNDARY.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor for the IDCRV defined above.  It defines a fraction of the changes required for the predicted initial blank shape.
   For example, if SF is set to 0.0 � the corresponding IDCRV will be excluded from the calculation (although the original initial curve still will be output);
   on the other hand, if SF is set to  1.0 , full change will be applied to obtain the modified initial blank that reflects the forming process.
   A SF of 0.5 will apply 50% of the changes required to map the initial blank.  This feature is especially important for inner holes that are small and hole boundary expansions are large,
   so the predicted initial hole can avoid  crisscross  situation.  An example is provided in Scale Factor and Symmetric Plane.
















   ..
       !! processed by numpydoc !!

.. py:property:: offx
   :type: float


   
   Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
   Input values of OFFX, OFFY and OFFZ helps establish one-to-one correspondence between each target curve and formed curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: offy
   :type: float


   
   Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
   Input values of OFFX, OFFY and OFFZ helps establish one-to-one correspondence between each target curve and formed curve.
















   ..
       !! processed by numpydoc !!

.. py:property:: offz
   :type: float


   
   Get or set the Translational move of the target curve.  This is useful when multiple target curves (e.g. holes) and formed curves are far away from each other.
   Input values of OFFX, OFFY and OFFZ helps establish one-to-one correspondence between each target curve and formed curve.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'BLANKSIZE_SCALE_FACTOR'






