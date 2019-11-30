with Tropos;

with Hera.Db;

package Hera.Ships.Configure is

   procedure Configure_Design (Design_Config : Tropos.Configuration);
   procedure Configure_Equipment (Equipment_Config : Tropos.Configuration);

   procedure Create_Ship
     (System        : Hera.Db.Star_System_Reference;
      Owner         : Hera.Db.Agent_Reference;
      Ship_Name     : String;
      Design_Name   : String;
      Configuration : String);

end Hera.Ships.Configure;
