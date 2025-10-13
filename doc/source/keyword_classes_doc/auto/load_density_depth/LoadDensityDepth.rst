





:class:`LoadDensityDepth`
=========================


.. py:class:: load_density_depth.LoadDensityDepth(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_DENSITY_DEPTH keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadDensityDepth

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part set ID, see *SET_PART.
          * - :py:attr:`~gc`
            - Get or set the Gravitational acceleration value.
          * - :py:attr:`~dir`
            - Get or set the Direction of loading:
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID defining density versus depth, see *DEFINE_CURVE.


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

    from load_density_depth import LoadDensityDepth

Property detail
---------------

.. py:property:: psid
   :type: int


   
   Get or set the Part set ID, see *SET_PART.
   EQ.0: all parts are initialized.
















   ..
       !! processed by numpydoc !!

.. py:property:: gc
   :type: float


   
   Get or set the Gravitational acceleration value.
















   ..
       !! processed by numpydoc !!

.. py:property:: dir
   :type: int


   
   Get or set the Direction of loading:
   EQ.1: global x (default),
   EQ.2: global y,
   EQ.3: global z.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID defining density versus depth, see *DEFINE_CURVE.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'DENSITY_DEPTH'






