with Hera.Factions;

with Hera.UI.Models.Data_Source;

with Hera.Db.Star;
with Hera.Db.Star_System;

package body Hera.UI.Models.Star_System is

   type Star_System_Model_Type is
     new Root_Hera_Model with
      record
         Star_System : Hera.Db.Star_System_Reference :=
           Hera.Db.Null_Star_System_Reference;
      end record;

   overriding procedure Start
     (Model     : in out Star_System_Model_Type;
      User      : Hera.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : Star_System_Model_Type)
      return String
   is ("star-system");

   overriding function Default_View_Name
     (Model : Star_System_Model_Type)
      return String
   is ("System");

   overriding function Changed
     (Model : Star_System_Model_Type)
      return Boolean;

   overriding function Handle
     (Model   : in out Star_System_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding function Get
     (Model   : Star_System_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   -------------
   -- Changed --
   -------------

   overriding function Changed
     (Model : Star_System_Model_Type)
      return Boolean
   is
      pragma Unreferenced (Model);
   begin
      return True;
   end Changed;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Model   : Star_System_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client);

      Result : Json.Json_Object;
   begin
      Result.Set_Property
        ("title", Hera.Db.Star_System.Get (Model.Star_System).Name);
      Result.Set_Property
        ("systemName", Hera.Db.Star_System.Get (Model.Star_System).Name);
      Result.Set_Property
        ("primary",
         Serialize
           (Hera.Db.Star.First_By_Star_System (Model.Star_System),
            Request));
      return Result;

   end Get;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Star_System_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Model.Get (State, Client, Request);
   end Handle;

   -----------------------
   -- Star_System_Model --
   -----------------------

   function Star_System_Model return Root_Hera_Model'Class is
   begin
      return Model : Star_System_Model_Type;
   end Star_System_Model;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out Star_System_Model_Type;
      User      : Hera.Db.User_Reference;
      Arguments : String)
   is
      Faction  : constant Hera.Factions.Faction_Type'Class :=
        Hera.Factions.Get_User_Faction (User);

   begin
      if Arguments = "" then
         Model.Star_System := Faction.Capital_System;
      else
         Model.Star_System :=
           Hera.Db.Star_System.First_Reference_By_Name (Arguments);
      end if;
   end Start;

end Hera.UI.Models.Star_System;
