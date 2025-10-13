





:class:`EmContactSubdom`
========================


.. py:class:: em_contact_subdom.EmContactSubdom(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA EM_CONTACT_SUBDOM keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: EmContactSubdom

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sdtype`
            - Get or set the Subdomain definition type:
          * - :py:attr:`~mvtype`
            - Get or set the Movement type of subdomain:
          * - :py:attr:`~lcidx_nid`
            - Get or set the Time dependent load curve ID for the translational velocity in the X direction for MVTYPE = 1, Node ID for MVTYPE = 2.
          * - :py:attr:`~lcidy`
            - Get or set the Time dependent load curve IDs for MVTYPE = 1 in the Y directions.
          * - :py:attr:`~lcidz`
            - Get or set the Time dependent load curve IDs for MVTYPE = 1 in the Y directions.
          * - :py:attr:`~r`
            - Get or set the Radius of the sphere if SDTYPE = 3 or the cylinder if SDTYPE = 2.
          * - :py:attr:`~pminx`
            - Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.
          * - :py:attr:`~pminy`
            - Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.
          * - :py:attr:`~pminz`
            - Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.
          * - :py:attr:`~pmaxx`
            - Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.
          * - :py:attr:`~pmaxy`
            - Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.
          * - :py:attr:`~pmaxz`
            - Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.


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

    from em_contact_subdom import EmContactSubdom

Property detail
---------------

.. py:property:: sdtype
   :type: int


   
   Get or set the Subdomain definition type:
   EQ.1: Defined by box
   EQ.2: Defined by cylinder.
   EQ.3: Defined by sphere.
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: mvtype
   :type: int


   
   Get or set the Movement type of subdomain:
   EQ.0: Static subdomain (Default).
   EQ.1: Domain translates in the three directions by the velocities given by LCIDX,LCIDY,LCIDZ.
   EQ.2: Domain follows the displacements of the node ID given by NID.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidx_nid
   :type: Optional[int]


   
   Get or set the Time dependent load curve ID for the translational velocity in the X direction for MVTYPE = 1, Node ID for MVTYPE = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidy
   :type: Optional[int]


   
   Get or set the Time dependent load curve IDs for MVTYPE = 1 in the Y directions.
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: lcidz
   :type: Optional[int]


   
   Get or set the Time dependent load curve IDs for MVTYPE = 1 in the Y directions.
   .
















   ..
       !! processed by numpydoc !!

.. py:property:: r
   :type: Optional[float]


   
   Get or set the Radius of the sphere if SDTYPE = 3 or the cylinder if SDTYPE = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pminx
   :type: Optional[float]


   
   Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pminy
   :type: Optional[float]


   
   Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pminz
   :type: Optional[float]


   
   Get or set the Point of minimum coordinates if SDTYPE = 1. Origin point if SDTYPE = 3. Axis head point if SDTYPE = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pmaxx
   :type: Optional[float]


   
   Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pmaxy
   :type: Optional[float]


   
   Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.
















   ..
       !! processed by numpydoc !!

.. py:property:: pmaxz
   :type: Optional[float]


   
   Get or set the Point of maximum coordinates if SDTYPE = 1. Axis tail point if SDTYPE = 2.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'EM'


.. py:attribute:: subkeyword
   :value: 'CONTACT_SUBDOM'






