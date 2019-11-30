with Hera.Managers.Ships;

package body Hera.Managers.Loader is

   -----------------------
   -- Register_Managers --
   -----------------------

   procedure Register_Managers is
   begin
      Register.Insert
        ("default-trade",
         Hera.Managers.Ships.Default_Trade_Manager'Access);
   end Register_Managers;

end Hera.Managers.Loader;
