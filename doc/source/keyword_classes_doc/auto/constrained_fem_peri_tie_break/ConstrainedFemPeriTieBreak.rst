





:class:`ConstrainedFemPeriTieBreak`
===================================


.. py:class:: constrained_fem_peri_tie_break.ConstrainedFemPeriTieBreak(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CONSTRAINED_FEM_PERI_TIE_BREAK keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: ConstrainedFemPeriTieBreak

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~cid`
            - Get or set the Contact ID.
          * - :py:attr:`~msid`
            - Get or set the The FEM part ID.
          * - :py:attr:`~ssid`
            - Get or set the The peridynamic part ID.
          * - :py:attr:`~ft`
            - Get or set the The tensile pressure to break the tie.
          * - :py:attr:`~fs`
            - Get or set the The shear pressure to break the tie.


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

    from constrained_fem_peri_tie_break import ConstrainedFemPeriTieBreak

Property detail
---------------

.. py:property:: cid
   :type: Optional[int]


   
   Get or set the Contact ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: msid
   :type: Optional[int]


   
   Get or set the The FEM part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ssid
   :type: Optional[int]


   
   Get or set the The peridynamic part ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ft
   :type: int


   
   Get or set the The tensile pressure to break the tie.
















   ..
       !! processed by numpydoc !!

.. py:property:: fs
   :type: int


   
   Get or set the The shear pressure to break the tie.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CONSTRAINED'


.. py:attribute:: subkeyword
   :value: 'FEM_PERI_TIE_BREAK'






