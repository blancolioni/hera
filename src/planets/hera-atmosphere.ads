private with Ada.Containers.Doubly_Linked_Lists;

package Hera.Atmosphere is

   type Atmospheric_Gas is
     (Ar, Cl2, CH4, CO2, F2, H2, He, N2, NH3, O2, SO2);

   type Atmosphere_Type is tagged private;

   function Partial_Pressure
     (Atm : Atmosphere_Type;
      Gas : Atmospheric_Gas)
      return Non_Negative_Real;

private

   type Atmospheric_Component is
      record
         Gas     : Atmospheric_Gas;
         Partial : Unit_Real;
      end record;

   package Atmospheric_Component_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Atmospheric_Component);

   type Atmosphere_Type is tagged
      record
         Pressure   : Non_Negative_Real;
         Components : Atmospheric_Component_Lists.List;
      end record;

end Hera.Atmosphere;
