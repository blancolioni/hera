with Ada.Containers.Vectors;

with Hera.UI.Models.Data_Source;

with Hera.Color;
with Hera.Solar_System;

with Hera.Knowledge;

with Hera.Corporations;
with Hera.Planets;
with Hera.Star_Systems;
with Hera.Stars;

package body Hera.UI.Models.Galaxy is

   package System_Vectors is
     new Ada.Containers.Vectors
       (Positive, Hera.Star_Systems.Star_System_Type,
        Hera.Star_Systems."=");

   type Galaxy_Model_Type is new Root_Hera_Model with
      record
         X, Y, Z  : Real := 0.0;
         Systems  : System_Vectors.Vector;
      end record;

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      User      : Hera.UI.UI_Account;
      Arguments : String);

   overriding function Name
     (Model : Galaxy_Model_Type)
      return String
   is ("galaxy-data-source");

   overriding function Default_View_Name
     (Model : Galaxy_Model_Type)
      return String
   is ("Galaxy");

   overriding function Changed
     (Model : Galaxy_Model_Type)
      return Boolean
   is (False);

   overriding function Handle
     (Model   : in out Galaxy_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   overriding function Get
     (Model   : Galaxy_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class;

   ------------------------
   -- Galaxy_Model --
   ------------------------

   function Galaxy_Model
      return Root_Hera_Model'Class
   is
   begin
      return Model : Galaxy_Model_Type;
   end Galaxy_Model;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Model   : Galaxy_Model_Type;
      State   : State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
      pragma Unreferenced (State, Client, Request);

      Result  : Json.Json_Object;
      Systems : Json.Json_Array;
   begin
      Result.Set_Property
        ("title", "Hera Galaxy");

      for Item of Model.Systems loop
         declare
            System : Json.Json_Object;
            Star   : constant Hera.Stars.Star_Type :=
              Hera.Stars.Star_Type (Item.Primary);

            procedure Set (Name : String;
                           X    : Real);

            ---------
            -- Set --
            ---------

            procedure Set (Name : String;
                           X    : Real)
            is
            begin
               System.Set_Property (Name, Float (X));
            end Set;

         begin
            System.Set_Property ("id", String (Item.Identifier));
            System.Set_Property ("name", Item.Name);
            System.Set_Property
              ("color", Hera.Color.To_Html_String
                 (Hera.Stars.Star_Type (Item.Primary).Color));

            Set ("mass", Star.Mass / Hera.Solar_System.Solar_Mass);
            Set ("temperature",
                 Star.Temperature
                 / Hera.Solar_System.Solar_Surface_Temperature);
            Set ("x", Item.X - Model.X);
            Set ("y", Item.Y - Model.Y);
            Set ("z", Item.Z - Model.Z);
            System.Set_Property ("colonized", Star.Colonized);
            Systems.Append (System);
         end;
      end loop;

      Result.Set_Property ("systems", Systems);

      return Result;

   end Get;

   ------------
   -- Handle --
   ------------

   overriding function Handle
     (Model   : in out Galaxy_Model_Type;
      State   : in out State_Interface'Class;
      Client  : Client_Id;
      Request : Hera.Json.Json_Value'Class)
      return Hera.Json.Json_Value'Class
   is
   begin
      return Model.Get (State, Client, Request);
   end Handle;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      User      : Hera.UI.UI_Account;
      Arguments : String)
   is
      Corporation : constant Hera.Corporations.Corporation_Type :=
        Hera.Corporations.Get (User.User_Name);
      Home_Planet : constant Hera.Planets.Planet_Type :=
        Corporation.Home_Planet;
      Home_System : constant Hera.Star_Systems.Star_System_Type :=
        Home_Planet.System;

      function Closer
        (Left, Right : Hera.Star_Systems.Star_System_Type)
         return Boolean
      is (Hera.Star_Systems.Distance (Left, Home_System)
          < Hera.Star_Systems.Distance (Right, Home_System));

      package System_Sorting is
        new System_Vectors.Generic_Sorting (Closer);

   begin

      Root_Hera_Model (Model).Start (User, Arguments);

      Model.X := Home_System.X;
      Model.Y := Home_System.Y;
      Model.Z := Home_System.Z;

      declare
         procedure Add_System
           (System : Hera.Star_Systems.Star_System_Type);

         ----------------
         -- Add_System --
         ----------------

         procedure Add_System
           (System : Hera.Star_Systems.Star_System_Type)
         is
         begin
            if Corporation.Knows (System, Hera.Knowledge.Exists) then
               Model.Systems.Append (System);
            end if;
         end Add_System;

      begin
         Hera.Star_Systems.Iterate (Add_System'Access);
         System_Sorting.Sort (Model.Systems);
      end;

   end Start;

end Hera.UI.Models.Galaxy;
