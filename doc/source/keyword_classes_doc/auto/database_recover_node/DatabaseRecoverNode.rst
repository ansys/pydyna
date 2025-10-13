





:class:`DatabaseRecoverNode`
============================


.. py:class:: database_recover_node.DatabaseRecoverNode(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA DATABASE_RECOVER_NODE keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: DatabaseRecoverNode

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~psid`
            - Get or set the Part set ID of solid elements whose nodal stress will be recovered.
          * - :py:attr:`~iax`
            - Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
          * - :py:attr:`~iay`
            - Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
          * - :py:attr:`~iaz`
            - Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
          * - :py:attr:`~method`
            - Get or set the Method used to recover the nodal stress
          * - :py:attr:`~ivx`
            - Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
          * - :py:attr:`~ivy`
            - Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
          * - :py:attr:`~ivz`
            - Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files


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

    from database_recover_node import DatabaseRecoverNode

Property detail
---------------

.. py:property:: psid
   :type: Optional[int]


   
   Get or set the Part set ID of solid elements whose nodal stress will be recovered.
















   ..
       !! processed by numpydoc !!

.. py:property:: iax
   :type: str


   
   Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
   EQ.SMNPD: the minimum principal deviator stress
   EQ.SMNPR : the minimum principal stress
   EQ.SMXPD : the maximum principal deviator stress
   EQ.SMXPR : the maximum principal stress
   EQ.SMXSH : the maximum shear stress
   EQ.SPR : nodal pressure
   EQ.SVM : nodal von Mises stress
   EQ.SXX : nodal normal stress along x direction
   EQ.SYY : nodal normal stress along y direction
   EQ.SZZ : nodal normal stress along z direction
   EQ.SXY : nodal shear stress along x - y direction
   EQ.SYZ : nodal shear stress along y - z direction
   EQ.SZX : nodal shear stress along z - x direction
   For shell elements append either "B" or "T" to the input string to
   recover nodal stresses at the bottom or top layer of shell elements.
   For example, SPRT recovers the nodal pressure at the top layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: iay
   :type: str


   
   Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
   EQ.SMNPD: the minimum principal deviator stress
   EQ.SMNPR : the minimum principal stress
   EQ.SMXPD : the maximum principal deviator stress
   EQ.SMXPR : the maximum principal stress
   EQ.SMXSH : the maximum shear stress
   EQ.SPR : nodal pressure
   EQ.SVM : nodal von Mises stress
   EQ.SXX : nodal normal stress along x direction
   EQ.SYY : nodal normal stress along y direction
   EQ.SZZ : nodal normal stress along z direction
   EQ.SXY : nodal shear stress along x - y direction
   EQ.SYZ : nodal shear stress along y - z direction
   EQ.SZX : nodal shear stress along z - x direction
   For shell elements append either "B" or "T" to the input string to
   recover nodal stresses at the bottom or top layer of shell elements.
   For example, SPRT recovers the nodal pressure at the top layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: iaz
   :type: str


   
   Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
   EQ.SMNPD: the minimum principal deviator stress
   EQ.SMNPR : the minimum principal stress
   EQ.SMXPD : the maximum principal deviator stress
   EQ.SMXPR : the maximum principal stress
   EQ.SMXSH : the maximum shear stress
   EQ.SPR : nodal pressure
   EQ.SVM : nodal von Mises stress
   EQ.SXX : nodal normal stress along x direction
   EQ.SYY : nodal normal stress along y direction
   EQ.SZZ : nodal normal stress along z direction
   EQ.SXY : nodal shear stress along x - y direction
   EQ.SYZ : nodal shear stress along y - z direction
   EQ.SZX : nodal shear stress along z - x direction
   For shell elements append either "B" or "T" to the input string to
   recover nodal stresses at the bottom or top layer of shell elements.
   For example, SPRT recovers the nodal pressure at the top layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: method
   :type: int


   
   Get or set the Method used to recover the nodal stress
   EQ.0:   Zienkiewicz-Zhu's Superconvergent Patch Recovery method
   EQ.1:   Elemental extrapolation method.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivx
   :type: str


   
   Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
   EQ.SMNPD: the minimum principal deviator stress
   EQ.SMNPR : the minimum principal stress
   EQ.SMXPD : the maximum principal deviator stress
   EQ.SMXPR : the maximum principal stress
   EQ.SMXSH : the maximum shear stress
   EQ.SPR : nodal pressure
   EQ.SVM : nodal von Mises stress
   EQ.SXX : nodal normal stress along x direction
   EQ.SYY : nodal normal stress along y direction
   EQ.SZZ : nodal normal stress along z direction
   EQ.SXY : nodal shear stress along x - y direction
   EQ.SYZ : nodal shear stress along y - z direction
   EQ.SZX : nodal shear stress along z - x direction
   For shell elements append either "B" or "T" to the input string to
   recover nodal stresses at the bottom or top layer of shell elements.
   For example, SPRT recovers the nodal pressure at the top layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivy
   :type: str


   
   Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
   EQ.SMNPD: the minimum principal deviator stress
   EQ.SMNPR : the minimum principal stress
   EQ.SMXPD : the maximum principal deviator stress
   EQ.SMXPR : the maximum principal stress
   EQ.SMXSH : the maximum shear stress
   EQ.SPR : nodal pressure
   EQ.SVM : nodal von Mises stress
   EQ.SXX : nodal normal stress along x direction
   EQ.SYY : nodal normal stress along y direction
   EQ.SZZ : nodal normal stress along z direction
   EQ.SXY : nodal shear stress along x - y direction
   EQ.SYZ : nodal shear stress along y - z direction
   EQ.SZX : nodal shear stress along z - x direction
   For shell elements append either "B" or "T" to the input string to
   recover nodal stresses at the bottom or top layer of shell elements.
   For example, SPRT recovers the nodal pressure at the top layer.
















   ..
       !! processed by numpydoc !!

.. py:property:: ivz
   :type: str


   
   Get or set the Meaning of "x/y/z-Acceleration" or "x/y/z-Velocity" in d3plot and d3thdt output files
   EQ.SMNPD: the minimum principal deviator stress
   EQ.SMNPR : the minimum principal stress
   EQ.SMXPD : the maximum principal deviator stress
   EQ.SMXPR : the maximum principal stress
   EQ.SMXSH : the maximum shear stress
   EQ.SPR : nodal pressure
   EQ.SVM : nodal von Mises stress
   EQ.SXX : nodal normal stress along x direction
   EQ.SYY : nodal normal stress along y direction
   EQ.SZZ : nodal normal stress along z direction
   EQ.SXY : nodal shear stress along x - y direction
   EQ.SYZ : nodal shear stress along y - z direction
   EQ.SZX : nodal shear stress along z - x direction
   For shell elements append either "B" or "T" to the input string to
   recover nodal stresses at the bottom or top layer of shell elements.
   For example, SPRT recovers the nodal pressure at the top layer.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'DATABASE'


.. py:attribute:: subkeyword
   :value: 'RECOVER_NODE'






