





:class:`EmExternalField`
========================


.. py:class:: em_external_field.EmExternalField(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_EXTERNAL_FIELD keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmExternalField

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~fieldid`
            - Get or set the External Field ID.
          * - :py:attr:`~ftype`
            - Get or set the Field type:
          * - :py:attr:`~fdef`
            - Get or set the Field defined by:
          * - :py:attr:`~lcidx`
            - Get or set the Load curve ID defining the X component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
          * - :py:attr:`~lcidy`
            - Get or set the Load curve ID defining the Y component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
          * - :py:attr:`~lcidz`
            - Get or set the Load curve ID defining the Z component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.


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

    from em_external_field import EmExternalField

Property detail
---------------

.. py:property:: fieldid
   :type: Optional[int]


   
   Get or set the External Field ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: ftype
   :type: int


   
   Get or set the Field type:
   EQ.1: Magnetic field.
   EQ.2: Electric field.
   EQ.3: charge density (resistive heating solver only).
















   ..
       !! processed by numpydoc !!

.. py:property:: fdef
   :type: int


   
   Get or set the Field defined by:
   EQ.1:Load curves.
   EQ.2: define function (FTYPE = 3 only). If a define function is used, the following parameters are accepted : x, y, z,time, emdt, pot, curr, sigma.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidx
   :type: Optional[int]


   
   Get or set the Load curve ID defining the X component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: Optional[int]


   
   Get or set the Load curve ID defining the Y component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: Optional[int]


   
   Get or set the Load curve ID defining the Z component of the field function of time for FTYPE = 1. For FTYPE = 3, only LCIDY is used and should be a simple a load curve or define function ID.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'EXTERNAL_FIELD'






