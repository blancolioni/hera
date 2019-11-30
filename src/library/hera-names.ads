with Tropos;

package Hera.Names is

   procedure Configure_Names
     (Config : Tropos.Configuration);

   function Random_Colony_Name return String;
   function Random_Corporate_Name return String;

end Hera.Names;
