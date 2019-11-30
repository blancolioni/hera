package Hera.Atmosphere.Configure is

   type Atmosphere_Builder is tagged private;

   function Vacuum return Atmosphere_Builder;

   procedure Add_Component
     (Atm     : in out Atmosphere_Builder;
      Gas     : Atmospheric_Gas;
      Partial : Unit_Real);

   procedure Scale_Component
     (Atm   : in out Atmosphere_Builder;
      Gas   : Atmospheric_Gas;
      Scale : Non_Negative_Real);

   function Partial
     (Atm : Atmosphere_Builder;
      Gas : Atmospheric_Gas)
      return Unit_Real;

   type Normalization_Strategy is (Change_First_Gas,
                                   Change_All_Gases);

   procedure Normalize
     (Builder : in out Atmosphere_Builder;
      Strategy : Normalization_Strategy);

   function To_Atmosphere
     (Builder  : Atmosphere_Builder'Class;
      Pressure : Non_Negative_Real)
      return Atmosphere_Type;

private

   function More (Left, Right : Atmospheric_Component) return Boolean
   is (Left.Partial > Right.Partial);

   package Atmospheric_Sorting is
     new Atmospheric_Component_Lists.Generic_Sorting (More);

   type Atmosphere_Builder is tagged
      record
         List : Atmospheric_Component_Lists.List;
      end record;

   function Vacuum return Atmosphere_Builder
   is (List => <>);

end Hera.Atmosphere.Configure;
