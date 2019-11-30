package body Hera.Atmosphere is

   ----------------------
   -- Partial_Pressure --
   ----------------------

   function Partial_Pressure
     (Atm : Atmosphere_Type;
      Gas : Atmospheric_Gas)
      return Non_Negative_Real
   is
   begin
      for Component of Atm.Components loop
         if Component.Gas = Gas then
            return Component.Partial * Atm.Pressure;
         end if;
      end loop;
      return 0.0;
   end Partial_Pressure;

end Hera.Atmosphere;
