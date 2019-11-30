with WL.Localisation;

--  with Hera.Calendar;
with Hera.Color;
--  with Hera.Constants;
--  with Hera.Orbits;
with Hera.Solar_System;

with Hera.Factions;
with Hera.Worlds;

with Hera.Db.Atmosphere;
with Hera.Db.Colony;
with Hera.Db.Faction;
with Hera.Db.Gas;
--  with Hera.Db.Massive_Object;
with Hera.Db.Palette_Entry;
with Hera.Db.Ship;
with Hera.Db.World;

package body Hera.UI.Models.Worlds is

   type World_Model_Type is
     new Hera.UI.Models.Root_Hera_Model with
      record
         World  : Hera.Db.World_Reference :=
           Hera.Db.Null_World_Reference;
         Colony : Hera.Db.Colony_Reference :=
           Hera.Db.Null_Colony_Reference;
      end record;

   overriding function Handle
     (Model   : in out World_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding function Get
     (Model   : World_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding procedure Start
     (Model     : in out World_Model_Type;
      User      : Hera.Db.User_Reference;
      Arguments : String);

   overriding function Changed
     (Model : World_Model_Type)
      return Boolean
   is (True);

   overriding function Name
     (Model : World_Model_Type)
      return String
   is ("world");

   overriding function Default_View_Name
     (Model : World_Model_Type)
      return String
   is ("World");

   ---------
   -- Get --
   ---------

   overriding function Get
     (Model   : World_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client);
      W : constant Hera.Db.World.World_Type :=
        Hera.Db.World.Get (Model.World);

      Result : Json.Json_Object;
   begin
      Result.Set_Property ("title", W.Name);
      Result.Set_Property
        ("world",
         Serialize (W, Request));
      return Result;
   end Get;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out World_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client, Request);
      World : constant Hera.Db.World.World_Type :=
        Hera.Db.World.Get (Model.World);

      Data  : Json.Json_Object;

      procedure Prop
        (Name : String;
         Value : Real);

      procedure Prop
        (Name  : String;
         Value : String);

      ----------
      -- Prop --
      ----------

      procedure Prop
        (Name  : String;
         Value : Real)
      is
      begin
         Data.Set_Property (Name, Json.Float_Value (Float (Value)));
      end Prop;

      ----------
      -- Prop --
      ----------

      procedure Prop
        (Name  : String;
         Value : String)
      is
      begin
         Data.Set_Property (Name, Value);
      end Prop;

      use Hera.Solar_System;

   begin
      Prop ("name", World.Name);
--        Prop ("category", Hera.Db.World_Category'Image (World.Category));
      Prop ("mass", World.Mass / Earth_Mass);
      Prop ("radius", World.Radius / Earth_Radius);
      Prop ("density", World.Density / Earth_Density);
      Prop ("orbit", World.Semimajor_Axis / Earth_Orbit);
      Prop ("day", World.Rotation_Period / 3600.0);
      Prop ("tilt", World.Tilt);
      Prop ("gravity", World.Surface_Gravity / Earth_Gravity);
--        Prop ("surfaceTemperature",
--              World.Surface_Temperature
--              - Hera.Constants.Freezing_Point_Of_Water);

      declare
         Atmosphere : Json.Json_Array;
      begin
         for Atm of Hera.Db.Atmosphere.Select_By_World (Model.World) loop
            declare
               Gas : constant Hera.Db.Gas.Gas_Type :=
                 Hera.Db.Gas.Get (Atm.Gas);
               Gas_Object : Json.Json_Object;
            begin
               Gas_Object.Set_Property
                 ("name", WL.Localisation.Local_Text (Gas.Tag));
               Gas_Object.Set_Property ("formula", Gas.Formula);
               Gas_Object.Set_Property
                 ("fraction", Json.Float_Value (Float (Atm.Percentage)));
               Atmosphere.Append (Gas_Object);
            end;
         end loop;
         Data.Set_Property ("atmosphere", Atmosphere);
      end;

      declare
         Palette : Json.Json_Array;
      begin
         for Item of
           Hera.Db.Palette_Entry.Select_By_Palette (World.Palette)
         loop
            declare
               Palette_Object : Json.Json_Object;
            begin
               Palette_Object.Set_Property
                 ("r", Json.Float_Value (Float (Item.Red)));
               Palette_Object.Set_Property
                 ("g", Json.Float_Value (Float (Item.Green)));
               Palette_Object.Set_Property
                 ("b", Json.Float_Value (Float (Item.Blue)));
               Palette.Append (Palette_Object);
            end;
         end loop;
         Data.Set_Property ("palette", Palette);
      end;

      declare
         Ships : Json.Json_Array;
      begin
         for Ship of Hera.Db.Ship.Select_By_World (Model.World) loop
            declare
               Faction     : constant Hera.Db.Faction.Faction_Type :=
                 Hera.Db.Faction.Get (Ship.Faction);
               Ship_Object : Json.Json_Object;
            begin
               Ship_Object.Set_Property ("name", Ship.Name);
               Ship_Object.Set_Property
                 ("owner", Faction.Name);
               Ship_Object.Set_Property
                 ("color",
                  Hera.Color.To_Html_String
                    (Faction.Red, Faction.Green, Faction.Blue));
               Ship_Object.Set_Property
                 ("orbit", Float (Ship.Semimajor_Axis / World.Radius));
               Ship_Object.Set_Property
                 ("inclination", 0.0);
               Ships.Append (Ship_Object);
            end;
         end loop;
         Data.Set_Property ("ships", Ships);
      end;

      return Result : Json.Json_Object do
         Result.Set_Property ("world", Data);
      end return;

   end Handle;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out World_Model_Type;
      User      : Hera.Db.User_Reference;
      Arguments : String)
   is
      use Hera.Db;
      Name : constant String :=
        (if Arguments = ""
         then Hera.Worlds.Name
           (Hera.Factions.Get_User_Faction (User).Capital_World)
         else Arguments);
      World : constant Hera.Db.World_Reference :=
        Hera.Db.World.First_Reference_By_Name (Name);
      Colony : constant Hera.Db.Colony_Reference :=
        (if World = Null_World_Reference then Null_Colony_Reference
         else Hera.Db.Colony.Get_Reference_By_World (World));

   begin
      Model.World := World;
      Model.Colony := Colony;
   end Start;

   -----------------
   -- World_Model --
   -----------------

   function World_Model return Root_Hera_Model'Class is
   begin
      return Model : World_Model_Type;
   end World_Model;

end Hera.UI.Models.Worlds;
