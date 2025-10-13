





:class:`InterfaceCompensation`
==============================


.. py:class:: interface_compensation.InterfaceCompensation(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA INTERFACE_COMPENSATION keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InterfaceCompensation

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~stage`
            - Get or set the Stage of this simulation in overall process. Stamping and springback must be finished before compensation can be performed.
          * - :py:attr:`~psidt`
            - Get or set the Part set ID including all tool parts(not required for STAGE=2)
          * - :py:attr:`~psidb`
            - Get or set the Part set ID including all blank(sheet)parts.
          * - :py:attr:`~smooth`
            - Get or set the Extrapolation method for tool surfaces outside final part (STAGE=3 only) A negative value can be used if undercutting occurs.
          * - :py:attr:`~scale`
            - Get or set the Compensation scale factor (STAGE=3 only).
          * - :py:attr:`~dbname`
            - Get or set the File name for binary compensation database(DEFAULT=lscomp)
          * - :py:attr:`~outname`
            - Get or set the File name for keyword output containing new tolls(STAGE=3 only,DEFAULT=lstool)


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

    from interface_compensation import InterfaceCompensation

Property detail
---------------

.. py:property:: stage
   :type: int


   
   Get or set the Stage of this simulation in overall process. Stamping and springback must be finished before compensation can be performed.
   EQ.1: stamping
   EQ.2: springback
   EQ.3: compensation(generate new tools).
















   ..
       !! processed by numpydoc !!

.. py:property:: psidt
   :type: Optional[int]


   
   Get or set the Part set ID including all tool parts(not required for STAGE=2)
















   ..
       !! processed by numpydoc !!

.. py:property:: psidb
   :type: Optional[int]


   
   Get or set the Part set ID including all blank(sheet)parts.
















   ..
       !! processed by numpydoc !!

.. py:property:: smooth
   :type: int


   
   Get or set the Extrapolation method for tool surfaces outside final part (STAGE=3 only) A negative value can be used if undercutting occurs.
   EQ.1: Preserve boundary slope.
   EQ.2:Zero slope.
   EQ.3: Smoothing method A(DEFAULT).
   EQ.4: Smoothing method B.
   EQ.5: Smoothing method c.
















   ..
       !! processed by numpydoc !!

.. py:property:: scale
   :type: float


   
   Get or set the Compensation scale factor (STAGE=3 only).
















   ..
       !! processed by numpydoc !!

.. py:property:: dbname
   :type: str


   
   Get or set the File name for binary compensation database(DEFAULT=lscomp)
















   ..
       !! processed by numpydoc !!

.. py:property:: outname
   :type: str


   
   Get or set the File name for keyword output containing new tolls(STAGE=3 only,DEFAULT=lstool)
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'INTERFACE'


.. py:attribute:: subkeyword
   :value: 'COMPENSATION'






