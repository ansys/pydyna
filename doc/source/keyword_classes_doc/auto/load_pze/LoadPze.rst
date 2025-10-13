





:class:`LoadPze`
================


.. py:class:: load_pze.LoadPze(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA LOAD_PZE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: LoadPze

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~setid`
            - Get or set the Set ID for the SET keyword option: Set noe, set segment.
          * - :py:attr:`~lcid`
            - Get or set the Load curve gives concentrated charge (in electric charge units) or distributed electric charge (in unit of electric charge per unit area) vs. time
          * - :py:attr:`~sf`
            - Get or set the Scale factor on curve or constant electric charge if LCID = 0..
          * - :py:attr:`~setyp`
            - Get or set the Type of SETID


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

    from load_pze import LoadPze

Property detail
---------------

.. py:property:: setid
   :type: Optional[int]


   
   Get or set the Set ID for the SET keyword option: Set noe, set segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve gives concentrated charge (in electric charge units) or distributed electric charge (in unit of electric charge per unit area) vs. time
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Scale factor on curve or constant electric charge if LCID = 0..
















   ..
       !! processed by numpydoc !!

.. py:property:: setyp
   :type: str


   
   Get or set the Type of SETID
   EQ.NSET:SETID is a node set.
   EQ.SEGSET : SETID is a segment set
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'LOAD'


.. py:attribute:: subkeyword
   :value: 'PZE'






