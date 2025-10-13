





:class:`DefineBoxCoarsen`
=========================


.. py:class:: define_box_coarsen.DefineBoxCoarsen(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DEFINE_BOX_COARSEN keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DefineBoxCoarsen

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~boxid`
            - Get or set the Box ID. A unique number must be defined.
          * - :py:attr:`~xmn`
            - Get or set the Minimum x-coordinate.
          * - :py:attr:`~xmx`
            - Get or set the Maximum x-coordinate.
          * - :py:attr:`~ymn`
            - Get or set the Minimum y-coordinate.
          * - :py:attr:`~ymx`
            - Get or set the Maximum y-coordinate.
          * - :py:attr:`~zmn`
            - Get or set the Minimum z-coordinate.
          * - :py:attr:`~zmx`
            - Get or set the Maximum z-coordinate.
          * - :py:attr:`~iflag`
            - Get or set the Flag for protecting elements inside or outside of box:
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

    from define_box_coarsen import DefineBoxCoarsen

Property detail
---------------

.. py:property:: boxid
   :type: Optional[int]


   
   Get or set the Box ID. A unique number must be defined.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmn
   :type: float


   
   Get or set the Minimum x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: xmx
   :type: float


   
   Get or set the Maximum x-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymn
   :type: float


   
   Get or set the Minimum y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: ymx
   :type: float


   
   Get or set the Maximum y-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmn
   :type: float


   
   Get or set the Minimum z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: zmx
   :type: float


   
   Get or set the Maximum z-coordinate.
















   ..
       !! processed by numpydoc !!

.. py:property:: iflag
   :type: int


   
   Get or set the Flag for protecting elements inside or outside of box:
   EQ.0: Elements outside box can not be coarsened,
   EQ.1: Elements inside box can not be coarsened.
















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
   :value: 'BOX_COARSEN'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





