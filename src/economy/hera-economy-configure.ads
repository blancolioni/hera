with Tropos;

with Hera.Db;

package Hera.Economy.Configure is

   procedure Create_Sector
     (Sector_Config : Tropos.Configuration);

   function Create_Economy
     (Economy_Name   : String;
      Economy_Config : Tropos.Configuration)
      return Hera.Db.Economy_Reference;

end Hera.Economy.Configure;
